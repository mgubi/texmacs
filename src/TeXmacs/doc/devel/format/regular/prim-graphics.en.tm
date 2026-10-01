<TeXmacs|1.0.7.7>

<style|tmdoc>

<\body>
  <tmdoc-title|Graphics primitives>

  This section briefly describes the primitives for images, graphics and
  animations. Most of them are typeset by the routines in
  <verbatim|Typeset/Concat/concat_graphics.cpp>,
  <verbatim|Typeset/Concat/concat_animate.cpp> and
  <verbatim|Typeset/Concat/concat_gui.cpp>; their arities are declared in
  <verbatim|Data/Drd/drd_std.cpp>. The rendering of graphical objects is
  controlled by many environment variables, such as <src-var|color>,
  <src-var|fill-color>, <src-var|line-width>, <src-var|dash-style>,
  <src-var|arrow-begin>, <src-var|arrow-end>, <src-var|point-style>,
  <src-var|text-at-halign>, <src-var|text-at-valign>, <src-var|gr-frame>,
  <src-var|gr-geometry> and <src-var|gr-grid>.

  <paragraph|Images>

  <\explain>
    <explain-macro|image|url|width|height|x-offset|y-offset><explain-synopsis|include
    an image>
  <|explain>
    Include the image with the given <src-arg|url>, which is resolved
    relative to the current document (the suffixes <verbatim|.eps> and
    <verbatim|.pdf> are tried when the <src-arg|url> has none). Instead of a
    file name, the first argument may also be an embedded image of the form
    <explain-macro|tuple|<with|font-shape|right|<explain-macro|raw-data|data>>|suffix>.

    The <src-arg|width> and <src-arg|height> are lengths, in which the units
    <verbatim|w> and <verbatim|h> stand for the natural width and height of
    the image. When both are empty, the natural size is used; when only one
    of them is specified, the other one is computed so as to preserve the
    aspect ratio. The box is finally moved by <src-arg|x-offset> and
    <src-arg|y-offset>, in which <verbatim|w> and <verbatim|h> now stand for
    the width and height of the scaled image.
  </explain>

  <paragraph|Graphics>

  <\explain>
    <explain-macro|graphics|gr-content-1|<math|\<cdots\>>|gr-content-n><explain-synopsis|graphics
    container>
  <|explain>
    A <markup|graphics> is a picture made of the graphical objects
    <src-arg|gr-content-1> until <src-arg|gr-content-n>, which are drawn on
    top of each other. The coordinates of the graphical objects are
    expressed in the coordinate system given by the <src-var|gr-frame>
    environment variable, and the size of the picture is determined by
    <src-var|gr-geometry> (or computed automatically when
    <src-var|gr-auto-crop> is set).
  </explain>

  <\explain>
    <explain-macro|superpose|content-1|<math|\<cdots\>>|content-n><explain-synopsis|superposition>
  <|explain>
    Typeset <src-arg|content-1> until <src-arg|content-n> as atomic boxes,
    and draw them on top of each other, with the same origin.
  </explain>

  <\explain>
    <explain-macro|gr-group|gr-content-1|<math|\<cdots\>>|gr-content-n><explain-synopsis|group
    of graphical objects>
  <|explain>
    Groups several graphical objects, so that they can be manipulated as a
    whole in the graphics editor.
  </explain>

  <\explain>
    <explain-macro|gr-transform|gr-content|transformation><explain-synopsis|transform
    graphics>
  <|explain>
    Apply a linear <src-arg|transformation> to <src-arg|gr-content>. The
    transformation is evaluated and should be one of
    <explain-macro|tuple|rotation|angle>,
    <explain-macro|tuple|rotation|center|angle> (angles in degrees),
    <explain-macro|tuple|scaling|x-factor|y-factor>,
    <explain-macro|tuple|slanting|factor> or
    <explain-macro|tuple|linear|a|b|c|d>.
  </explain>

  <\explain>
    <explain-macro|gr-effect|content-1|<math|\<cdots\>>|content-n|effect><explain-synopsis|graphical
    effects>
  <|explain>
    Apply a graphical <src-arg|effect> to the boxes of <src-arg|content-1>
    until <src-arg|content-n>. The effect is evaluated and built from the
    effect primitives <markup|eff-move>, <markup|eff-magnify>,
    <markup|eff-bubble>, <markup|eff-crop>, <markup|eff-turbulence>,
    <markup|eff-fractal-noise>, <markup|eff-hatch>, <markup|eff-dots>,
    <markup|eff-gaussian>, <markup|eff-oval>, <markup|eff-rectangular>,
    <markup|eff-motion>, <markup|eff-blur>, <markup|eff-outline>,
    <markup|eff-thicken>, <markup|eff-erode>, <markup|eff-degrade>,
    <markup|eff-distort>, <markup|eff-gnaw>, <markup|eff-superpose>,
    <markup|eff-add>, <markup|eff-sub>, <markup|eff-mul>, <markup|eff-min>,
    <markup|eff-max>, <markup|eff-mix>, <markup|eff-normalize>,
    <markup|eff-monochrome>, <markup|eff-color-matrix>,
    <markup|eff-gradient>, <markup|eff-make-transparent>,
    <markup|eff-make-opaque>, <markup|eff-recolor> and <markup|eff-skin>.
    In an effect, the numbers <verbatim|0>, <verbatim|1>, <abbr|etc.>
    refer to the boxes of <src-arg|content-1>, <src-arg|content-2>,
    <abbr|etc.> (an empty string stands for the first box). The menus
    for inserting effects are only shown when the <verbatim|bitmap effects>
    preference is enabled.
  </explain>

  <\explain>
    <explain-macro|text-at|content|pos>

    <explain-macro|math-at|content|pos>

    <explain-macro|document-at|content|pos><explain-synopsis|content at a
    given position>
  <|explain>
    Place textual, mathematical or multi-paragraph <src-arg|content> at the
    point <src-arg|pos> of a graphics. The alignment of the content with
    respect to <src-arg|pos> is determined by the environment variables
    <src-var|text-at-halign> (<verbatim|left>, <verbatim|center> or
    <verbatim|right>) and <src-var|text-at-valign> (<verbatim|bottom>,
    <verbatim|base>, <verbatim|axis>, <verbatim|center> or <verbatim|top>).
  </explain>

  <\explain>
    <explain-macro|point|x|y><explain-synopsis|point>
  <|explain>
    A point with coordinates <src-arg|x> and <src-arg|y> (evaluated), in the
    coordinate system of the enclosing <markup|graphics>. A point also
    serves as a graphical object, rendered according to
    <src-var|point-style>, <src-var|point-size> and <src-var|point-border>.
  </explain>

  <\explain>
    <explain-macro|line|point-1|<math|\<cdots\>>|point-n>

    <explain-macro|cline|point-1|<math|\<cdots\>>|point-n><explain-synopsis|polygonal
    lines>
  <|explain>
    Polygonal line through the points <src-arg|point-1> until
    <src-arg|point-n>. The <markup|cline> variant is closed, so that it
    yields a polygon, which is filled with <src-var|fill-color>.
  </explain>

  <\explain>
    <explain-macro|arc|point-1|point-2|point-3>

    <explain-macro|carc|point-1|point-2|point-3><explain-synopsis|arcs and
    circles>
  <|explain>
    The <markup|arc> primitive draws the arc of a circle through the three
    given points, and <markup|carc> the entire circle through these points.
    When the points are aligned, a polygonal line is drawn instead.
  </explain>

  <\explain>
    <explain-macro|spline|point-1|<math|\<cdots\>>|point-n>

    <explain-macro|cspline|point-1|<math|\<cdots\>>|point-n><explain-synopsis|splines>
  <|explain>
    Open or closed spline curve through the points <src-arg|point-1> until
    <src-arg|point-n>. The variant <markup|spline*> is reserved, but not
    yet implemented.
  </explain>

  <\explain>
    <explain-macro|bezier|point-1|<math|\<cdots\>>|point-n>

    <explain-macro|cbezier|point-1|<math|\<cdots\>>|point-n>

    <explain-macro|smooth|point-1|<math|\<cdots\>>|point-n>

    <explain-macro|csmooth|point-1|<math|\<cdots\>>|point-n><explain-synopsis|Bezier
    curves>
  <|explain>
    Open and closed poly-Bezier curves. For <markup|bezier> and
    <markup|cbezier> the points alternate between points on the curve and
    control points; for <markup|smooth> and <markup|csmooth> the control
    points are computed automatically so as to yield a smooth curve through
    the given points.
  </explain>

  <\explain>
    <explain-macro|penscript|start|end|metadata|ink>

    <explain-macro|calligraphy|start|end|metadata|ink><explain-synopsis|handwriting>
  <|explain>
    These primitives represent handwritten strokes between the points
    <src-arg|start> and <src-arg|end>; the <src-arg|ink> argument contains
    the recorded points of the stroke.
  </explain>

  <\explain>
    <explain-macro|fill|<math|\<cdots\>>><explain-synopsis|filled region>
  <|explain>
    Reserved primitive, not yet implemented.
  </explain>

  <\explain>
    <explain-macro|box-info|content|query><explain-synopsis|dimensions of a
    box>
  <|explain>
    Typeset <src-arg|content> and return information about its box, as
    specified by the literal string <src-arg|query>. Each letter of the
    query adds an entry to the returned tuple: <verbatim|l>, <verbatim|b>,
    <verbatim|r>, <verbatim|t>, <verbatim|w> and <verbatim|h> for the left,
    bottom, right and top coordinates, the width and the height of the
    logical box, and the corresponding upper case letters for the ink box.
    If the query ends with a dot, such as <verbatim|w.>, a single length
    (in <verbatim|tmpt>) is returned instead of a tuple.
  </explain>

  <\explain>
    <explain-macro|frame-direct|point>

    <explain-macro|frame-inverse|point><explain-synopsis|coordinate
    conversions>
  <|explain>
    Convert a <src-arg|point> from the coordinate system of the current
    graphics frame into internal coordinates, or conversely.
  </explain>

  The primitives <markup|is-equal>, <markup|is-intersection>,
  <markup|on-curve>, <markup|on-text-border> and <markup|on-grid> are
  reserved for expressing geometric constraints, and the primitives
  <markup|transform-3d>, <markup|object-3d>, <markup|triangle-3d>,
  <markup|light-3d>, <markup|light-diffuse> and <markup|light-specular> for
  experimental three dimensional graphics.

  <paragraph|Fill patterns>

  <\explain>
    <explain-macro|pattern|url|width|height>

    <explain-macro|pattern|url|width|height|alternative><explain-synopsis|image
    pattern>
  <|explain>
    This primitive can be used as a color (for instance as the value of
    <src-var|bg-color> or <src-var|fill-color>): the region is filled with
    copies of the image <src-arg|url>, scaled to the given size. The
    optional <src-arg|alternative> is a plain color which is used when
    patterns are not supported. The <markup|gradient> primitive is reserved
    for gradients, but not yet implemented.
  </explain>

  <paragraph|Decorated boxes>

  <\explain>
    <explain-macro|ornament|body>

    <explain-macro|ornament|body|title><explain-synopsis|ornamented box>
  <|explain>
    Typeset <src-arg|body> inside an ornamental frame, with an optional
    <src-arg|title>. The shape and colors of the frame are determined by the
    <src-var|ornament-shape>, <src-var|ornament-title-style>,
    <src-var|ornament-border>, <src-var|ornament-color> and related
    environment variables. This primitive is used by the style packages for
    framed and highlighted environments.
  </explain>

  <\explain>
    <explain-macro|canvas|x1|y1|x2|y2|x-scroll|y-scroll|body><explain-synopsis|scrollable
    region>
  <|explain>
    Typeset <src-arg|body> inside a scrollable region with the given limits
    <src-arg|x1>, <src-arg|y1>, <src-arg|x2>, <src-arg|y2>, and scroll
    positions <src-arg|x-scroll> and <src-arg|y-scroll>. The appearance is
    controlled by <src-var|canvas-type> and the other <verbatim|canvas-*>
    environment variables.
  </explain>

  <\explain>
    <explain-macro|art-box|body|properties-1|<math|\<cdots\>>|properties-n><explain-synopsis|artistic
    box>
  <|explain>
    Typeset <src-arg|body> on top of an artistic background (for instance
    made of images), described by the tuples <src-arg|properties-1> until
    <src-arg|properties-n> of variable-value pairs. The properties
    <verbatim|lpadding>, <verbatim|rpadding>, <verbatim|bpadding> and
    <verbatim|tpadding> of the tuple starting with <verbatim|text> determine
    the padding around the <src-arg|body>.
  </explain>

  <paragraph|Animations>

  Animations are typeset as boxes which change over time; they are driven by
  <em|players> (<verbatim|Typeset/Concat/concat_animate.cpp>). Durations
  are specified as lengths with the units <verbatim|ms>, <verbatim|s>,
  <verbatim|msec>, <verbatim|sec>, <verbatim|min> and <verbatim|hr>.

  <\explain>
    <explain-macro|anim-constant|content|duration><explain-synopsis|content
    displayed during some time>
  <|explain>
    Display <src-arg|content> during the given <src-arg|duration>.
  </explain>

  <\explain>
    <explain-macro|anim-compose|anim-1|<math|\<cdots\>>|anim-n><explain-synopsis|sequential
    composition>
  <|explain>
    Play the animations <src-arg|anim-1> until <src-arg|anim-n> one after
    another.
  </explain>

  <\explain>
    <explain-macro|anim-repeat|anim><explain-synopsis|repeated animation>
  <|explain>
    Play the animation <src-arg|anim> repeatedly.
  </explain>

  <\explain>
    <explain-macro|anim-accelerate|anim|type><explain-synopsis|change the
    time flow>
  <|explain>
    Play <src-arg|anim> with a modified time flow. The <src-arg|type> may be
    <verbatim|reverse>, <verbatim|fade-in>, <verbatim|fade-out>,
    <verbatim|faded>, <verbatim|bump>, <verbatim|reverse-><em|type>, or
    <explain-macro|tuple|fixed|t> for a frozen animation.
  </explain>

  <\explain>
    <explain-macro|anim-translate|content|duration|start|end>

    <explain-macro|anim-progressive|content|duration|start|end><explain-synopsis|moving
    and progressively revealed content>
  <|explain>
    The <markup|anim-translate> primitive moves <src-arg|content> during
    <src-arg|duration> from the position <src-arg|start> to the position
    <src-arg|end>; positions are tuples <explain-macro|tuple|x|y>, where
    numbers are fractions of the size of the box and lengths are absolute
    offsets. The <markup|anim-progressive> primitive progressively reveals
    <src-arg|content>, by interpolating between the clipping rectangles
    <src-arg|start> and <src-arg|end>, which are tuples with four
    coordinates.
  </explain>

  <\explain>
    <explain-macro|anim-static|content|duration|step|extra>

    <explain-macro|anim-dynamic|content|duration|step|extra><explain-synopsis|computed
    animations>
  <|explain>
    Evaluate <src-arg|content> at regular time steps <src-arg|step> during
    <src-arg|duration>, and produce the corresponding
    <markup|anim-compose> of <markup|anim-constant> frames. During the
    evaluation, the primitives <explain-macro|anim-time> and
    <explain-macro|anim-portion> return the current time and the elapsed
    portion (between <verbatim|0> and <verbatim|1>) of the animation, and
    each <markup|morph> in <src-arg|content> is replaced by an interpolated
    value. The last argument is currently unused, and <markup|anim-dynamic>
    currently behaves like <markup|anim-static>.
  </explain>

  <\explain>
    <explain-macro|morph|<with|font-shape|right|<explain-macro|tuple|t-1|content-1>>|<math|\<cdots\>>|<with|font-shape|right|<explain-macro|tuple|t-n|content-n>>><explain-synopsis|interpolated
    content>
  <|explain>
    Inside a computed animation, a <markup|morph> evaluates to an
    interpolation between the <src-arg|content-i> whose times
    <src-arg|t-i> (numbers between <verbatim|0> and <verbatim|1>) surround
    the current animation portion. Numbers, lengths, colors,
    <markup|with> attributes, tables and graphics are interpolated
    recursively (<verbatim|Typeset/Env/env_animate.cpp>); other content is
    switched abruptly. A <src-arg|content-i> which is not a tuple is taken
    to be the content at time <verbatim|0> (first occurrence) or
    <verbatim|1> (second occurrence).
  </explain>

  <\explain>
    <explain-macro|video|url|width|height|duration|repeat>

    <explain-macro|sound|url><explain-synopsis|multimedia content>
  <|explain>
    Include a video or sound file. For videos, <src-arg|repeat> specifies
    whether the video should be played repeatedly (any value other than
    <verbatim|false>).
  </explain>

  <tmdoc-copyright|2004|Joris van der Hoeven|David Allouche>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>
