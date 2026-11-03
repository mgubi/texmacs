<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Graphics>

  The environment variables described in this section control the
  <hlink|graphical primitives|../regular/prim-graphics.en.tm> of <TeXmacs>.
  They fall into three groups: variables which determine the global layout
  of a <markup|graphics> object (its size, coordinate frame and grids),
  variables which determine the style of the individual graphical objects
  (points, curves, text, <abbr|etc.>), and variables with a <verbatim|gr->
  prefix which are used by the graphics editor for newly created objects.
  The built-in default values are defined in
  <source-link|src/Typeset/Env/env_default.cpp|src/Typeset/Env/env_default.cpp>.

  <section|Global layout of graphics>

  <\explain>
    <var-val|gr-geometry|<tuple|geometry|1par|0.6par|center>><explain-synopsis|size
    of the graphics>
  <|explain>
    The geometry of a <markup|graphics> object, as a tuple
    <explain-macro|tuple|geometry|width|height> or
    <explain-macro|tuple|geometry|width|height|valign>. The width and height
    are lengths. The optional vertical alignment is one of
    <verbatim|center> (default), <verbatim|top>, <verbatim|bottom> or
    <verbatim|axis> and determines how the graphics is placed with respect
    to the surrounding baseline.
  </explain>

  <\explain>
    <var-val|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>><explain-synopsis|coordinate
    frame>
  <|explain>
    The coordinate frame used for the points of the graphics, as a tuple
    <explain-macro|tuple|scale|unit|<with|font-shape|right|<explain-macro|tuple|x|y>>>.
    The <src-arg|unit> is the length of one unit of the user coordinates and
    <src-arg|x>, <src-arg|y> are the lengths which give the position of the
    origin. The length units <verbatim|gw> and <verbatim|gh> refer to the
    width and height of the graphics, as specified by
    <src-var|gr-geometry>, and <verbatim|gu> to the length of one user unit.
    By default, the origin is at the center of the graphics and one unit
    corresponds to <verbatim|1cm>.
  </explain>

  <\explain>
    <var-val|gr-grid|><explain-synopsis|visual grid>

    <var-val|gr-grid-aspect|<tuple|<tuple|axes|#808080>|<tuple|1|#c0c0c0>|<tuple|10|#e0e0ff>>><explain-synopsis|visual
    grid aspect>
  <|explain>
    The grid which is drawn in the background of the graphics. The empty
    string or <explain-macro|tuple|empty> means that no grid is drawn.
    Otherwise, the grid is one of

    <\itemize>
      <item><explain-macro|tuple|cartesian>,
      <explain-macro|tuple|cartesian|step> or
      <explain-macro|tuple|cartesian|center|step>;

      <item><explain-macro|tuple|polar>, <explain-macro|tuple|polar|step>,
      <explain-macro|tuple|polar|step|astep> or
      <explain-macro|tuple|polar|center|step|astep>, where
      <src-arg|astep> is the number of angular subdivisions (8 by default);

      <item><explain-macro|tuple|logarithmic>,
      <explain-macro|tuple|logarithmic|step>,
      <explain-macro|tuple|logarithmic|step|base> or
      <explain-macro|tuple|logarithmic|center|step|base> (the base is 10
      by default).
    </itemize>

    The <src-var|gr-grid-aspect> is a tuple of pairs
    <explain-macro|tuple|subdivision|color>, which specify the colors of the
    axes (subdivision <verbatim|axes>) and of the grid lines for each
    number of subdivisions of the unit step.
  </explain>

  <\explain>
    <var-val|gr-edit-grid|><explain-synopsis|editing grid>

    <var-val|gr-edit-grid-aspect|<tuple|<tuple|axes|#808080>|<tuple|1|#c0c0c0>|<tuple|10|#e0e0ff>>><explain-synopsis|editing
    grid aspect>
  <|explain>
    A second grid, with the same syntax as <src-var|gr-grid> and
    <src-var|gr-grid-aspect>, which is used by the graphics editor for
    snapping points when editing. It is not shown in the typeset document.
  </explain>

  <\explain>
    <var-val|gr-auto-crop|false><explain-synopsis|automatic cropping>

    <var-val|gr-crop-padding|1spc><explain-synopsis|padding for automatic
    cropping>
  <|explain>
    When <src-var|gr-auto-crop> is <verbatim|true>, the size of the graphics
    is not determined by <src-var|gr-geometry>, but by the bounding box of
    its graphical objects, enlarged by <src-var|gr-crop-padding> on each
    side.
  </explain>

  <\explain>
    <src-var|gr-transformation><explain-synopsis|3D transformation>
  <|explain>
    A <math|4\<times\>4> matrix, given as a tuple of four row tuples, which
    is applied to three-dimensional objects (<markup|transform-3d>,
    <markup|object-3d>, <markup|triangle-3d> and <markup|light-3d>) before
    they are projected onto the plane of the graphics. The default is the
    identity matrix.
  </explain>

  <\explain>
    <var-val|gr-mode|line><explain-synopsis|current editing mode>

    <var-val|gr-snap-distance|10px><explain-synopsis|snapping distance>
  <|explain>
    These variables are only used by the graphics editor. The
    <src-var|gr-mode> contains the current editing mode, usually as a tuple
    such as <explain-macro|tuple|edit|line>,
    <explain-macro|tuple|edit|text-at>,
    <explain-macro|tuple|group-edit|move> or
    <explain-macro|tuple|hand-edit|calligraphy> (a plain string <verbatim|m>
    is interpreted as <explain-macro|tuple|edit|m>). The
    <src-var|gr-snap-distance> is the maximal distance at which the cursor
    snaps to grid points and to other objects.
  </explain>

  <section|Style of graphical objects>

  The following variables are usually set using a <markup|with> tag around
  an individual graphical object. The graphics editor stores the properties
  of each object in this way.

  <\explain>
    <var-val|gid|default><explain-synopsis|graphical identifier>

    <var-val|anim-id|><explain-synopsis|animation identifier>
  <|explain>
    The <src-var|gid> attaches an identifier to a graphical object. It is
    used for maintaining constraints between graphical objects: a point
    which carries an identifier may be referred to from elsewhere in the
    graphics. The <src-var|anim-id> identifies corresponding objects in the
    successive frames of an animation, so that they can be morphed into
    each other.
  </explain>

  <\explain>
    <var-val|proviso|true><explain-synopsis|visibility condition>
  <|explain>
    When a graphical object is enclosed in a <markup|with> tag which sets
    <src-var|proviso> to (an expression evaluating to) <verbatim|false>, the
    object is not displayed.
  </explain>

  <\explain>
    <var-val|magnify|1><explain-synopsis|magnification of graphical
    objects>
  <|explain>
    An additional magnification factor for graphical objects. When
    typesetting the objects of a graphics, the current
    <src-var|magnification> is multiplied by this factor, which affects for
    instance the size of text inside <markup|text-at> objects.
  </explain>

  <\explain>
    <var-val|opacity|100%><explain-synopsis|opacity>
  <|explain>
    The opacity of both the lines and the filling of graphical objects. The
    value is either a percentage like
    <verbatim|50%>, or a number between <verbatim|0> and <verbatim|1>.
  </explain>

  <\explain>
    <var-val|color|black><explain-synopsis|line color>

    <var-val|fill-color|none><explain-synopsis|fill color>
  <|explain>
    The color <src-var|color> is used for drawing points and curves; the
    value <verbatim|none> means that no line is drawn. The
    <src-var|fill-color> is used for filling closed curves and the interior
    of points; the default value <verbatim|none> means that no filling takes
    place. Both colors may also be patterns.
  </explain>

  <\explain>
    <var-val|point-style|disk><explain-synopsis|point style>

    <var-val|point-size|2.5ln><explain-synopsis|point size>

    <var-val|point-border|1ln><explain-synopsis|border width of points>
  <|explain>
    The shape of points. Supported styles are <verbatim|disk>,
    <verbatim|round> (a circle, filled with the <src-var|fill-color>),
    <verbatim|square>, <verbatim|diamond>, <verbatim|triangle>,
    <verbatim|star>, <verbatim|plus>, <verbatim|cross> and
    <verbatim|none>. The <src-var|point-size> and <src-var|point-border>
    determine the radius of the point and the width of its border.
  </explain>

  <\explain>
    <var-val|line-width|1ln><explain-synopsis|line width>
  <|explain>
    The width of lines and curves.
  </explain>

  <\explain>
    <var-val|line-portion|1><explain-synopsis|portion of the curve>
  <|explain>
    A number between <verbatim|0> and <verbatim|1>, which specifies which
    initial portion of a curve is actually drawn. This is mainly useful in
    animations.
  </explain>

  <\explain>
    <var-val|dash-style|none><explain-synopsis|dash style>

    <var-val|dash-style-unit|5ln><explain-synopsis|unit of dash motif>
  <|explain>
    The <src-var|dash-style> is either <verbatim|none> (a plain line), a
    string of <verbatim|0> and <verbatim|1> characters like
    <verbatim|11100>, which specifies a periodic pattern of dashes, or one of
    the decorative motifs <verbatim|zigzag>, <verbatim|wave>,
    <verbatim|pulse>, <verbatim|loops> and <verbatim|meander>. The
    <src-var|dash-style-unit> is the length of one character of the pattern
    or of one period of the motif. For motifs, it may also be a tuple
    <explain-macro|tuple|horizontal-unit|vertical-unit>, so as to control
    the amplitude separately.
  </explain>

  <\explain>
    <var-val|arrow-begin|none><explain-synopsis|arrow at the beginning>

    <var-val|arrow-end|none><explain-synopsis|arrow at the end>

    <var-val|arrow-length|5ln><explain-synopsis|arrow length>

    <var-val|arrow-height|5ln><explain-synopsis|arrow height>
  <|explain>
    Arrow heads at the beginning and the end of curves. Built-in arrow
    heads are <verbatim|\<less\>>, <verbatim|\<gtr\>>,
    <verbatim|\<less\>\|>, <verbatim|\|\<gtr\>>,
    <verbatim|\<less\>\<less\>>, <verbatim|\<gtr\>\<gtr\>>, <verbatim|\|>
    and <verbatim|o>; <verbatim|none> or the empty string mean that no arrow
    is drawn. An arbitrary graphical object may also be given instead of a
    string. The <src-var|arrow-length> and <src-var|arrow-height> determine
    the size of the built-in arrow heads, along and across the curve.
  </explain>

  <\explain>
    <var-val|line-join|normal><explain-synopsis|line junctions>

    <var-val|line-caps|normal><explain-synopsis|line caps>

    <var-val|line-effects|none><explain-synopsis|line effects>

    <var-val|fill-style|plain><explain-synopsis|fill style>
  <|explain>
    These variables are recognized as graphical attributes by the editor,
    but they are not interpreted by the current typesetter.
  </explain>

  <\explain>
    <var-val|text-at-halign|left><explain-synopsis|horizontal alignment of
    text>

    <var-val|text-at-valign|base><explain-synopsis|vertical alignment of
    text>
  <|explain>
    The alignment of <markup|text-at> and <markup|math-at> objects with
    respect to their anchor point. The horizontal alignment is
    <verbatim|left>, <verbatim|center> or <verbatim|right>. The vertical
    alignment is <verbatim|base> (the baseline passes through the point),
    <verbatim|bottom>, <verbatim|axis>, <verbatim|center> or
    <verbatim|top>.
  </explain>

  <\explain>
    <var-val|text-at-repulse|off><explain-synopsis|repulsive margin>

    <var-val|text-at-snapping|1spc><explain-synopsis|snapping margin>
  <|explain>
    When <src-var|text-at-repulse> is a length instead of <verbatim|off>,
    the bounding box of a text object, enlarged by this length, becomes a
    \Pwhite zone\Q in which curves drawn afterwards are interrupted, so that
    labels remain readable. The <src-var|text-at-snapping> is the margin
    around text objects which is used for snapping and smart guides when
    editing.
  </explain>

  <\explain>
    <var-val|doc-at-valign|top><explain-synopsis|vertical alignment of
    long text>

    <var-val|doc-at-width|1par><explain-synopsis|width of long text>

    <var-val|doc-at-hmode|min><explain-synopsis|width mode of long text>

    <var-val|doc-at-ppsep|0fn><explain-synopsis|paragraph separation in long
    text>

    <var-val|doc-at-border|0ln><explain-synopsis|border of long text>

    <var-val|doc-at-padding|0spc><explain-synopsis|padding of long text>
  <|explain>
    Layout of <markup|document-at> objects, which contain multi-paragraph
    text. The <src-var|doc-at-valign> has the same possible values as
    <src-var|text-at-valign>, but defaults to <verbatim|top>. The body is
    typeset inside a one-cell table (see the <markup|paragraph-box> macro in
    <source-link|std-graphics.ts|TeXmacs/packages/standard/std-graphics.ts>), whose width and width mode
    (<src-var|table-width> and <src-var|table-hmode>) are given by
    <src-var|doc-at-width> and <src-var|doc-at-hmode>, whose borders and
    paddings are given by <src-var|doc-at-border> and
    <src-var|doc-at-padding>, and whose background is the
    <src-var|fill-color>. A non-empty <src-var|doc-at-ppsep> overrides
    <src-var|par-par-sep> inside the body.
  </explain>

  <\explain>
    <var-val|pen-enhance|gaussian><explain-synopsis|smoothing of hand
    drawings>

    <var-val|pen-style|default><explain-synopsis|pen for calligraphy>
  <|explain>
    These variables control hand-drawn curves (<markup|penscript> and
    <markup|calligraphy>). The <src-var|pen-enhance> is the smoothing method:
    <verbatim|gaussian> (or <verbatim|default>), <verbatim|bezier>, or any
    other value (like <verbatim|none>) for no smoothing. It may also be a
    tuple <explain-macro|tuple|method|strength>, where the strength is a
    number between <verbatim|0.2> and <verbatim|5>. For <markup|calligraphy>,
    the <src-var|pen-style> may be a tuple
    <explain-macro|tuple|oval|ratio|angle>, which specifies an oval pen with
    the given aspect ratio and angle (in degrees); the width of the pen is
    the <src-var|line-width>.
  </explain>

  <section|Properties for new objects>

  <\explain>
    <var-val|gr-color|default>

    <var-val|gr-line-width|default>

    ...<explain-synopsis|properties of new
    objects>
  <|explain>
    For each of the style variables <src-var|gid>, <src-var|anim-id>,
    <src-var|proviso>, <src-var|magnify>, <src-var|opacity>,
    <src-var|color>, <src-var|fill-color>, <src-var|point-style>, ...,
    <src-var|pen-style> of the previous section, there is a corresponding
    variable with a <verbatim|gr-> prefix (<src-var|gr-color>,
    <src-var|gr-fill-color>, <src-var|gr-point-style>, <abbr|etc.>). These
    variables are set on the <markup|graphics> tag by the graphics editor
    and hold the properties which will be given to newly created objects.
    The value <verbatim|default> means that no explicit value is attached to
    new objects, so that the value of the ordinary variable applies.
  </explain>

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
