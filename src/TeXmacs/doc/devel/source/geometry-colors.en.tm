<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Colors>

  <section|The color word>

  A <cpp|color> is a plain <cpp|unsigned int>
  (<verbatim|Kernel/Abstractions/basic.hpp>). In the normal <em|true color>
  mode (the global <cpp|true_colors>, which is <cpp|true> by default) it
  holds the four components in the order alpha, red, green, blue, from the
  most to the least significant byte:

  <\cpp-code>
    color rgb_color (int r, int g, int b, int a= 255);

    // = (a \<less\>\<less\> 24) + (r \<less\>\<less\> 16) + (g \<less\>\<less\> 8) + b
  </cpp-code>

  Components range from 0 to 255; alpha 255 is opaque and 0 fully
  transparent. <cpp|get_rgb_color> decodes a color word; there is also a
  variant returning an <cpp|array\<less\>int\<gtr\>> of four components,
  and <cpp|rgb_color (array\<less\>int\<gtr\>)> in which missing
  components default to 255.

  The other mode (<cpp|set_true_colors (false)>) is a relic of indexed
  displays: colors are then mapped to a palette of <cpp|CFACTOR>
  <math|\<times\>> <cpp|CFACTOR> <math|\<times\>> <cpp|CFACTOR> colors and
  <cpp|GREYS> grey levels (<cpp|set_color_attrs>, <cpp|get_color_attrs>),
  and the low 24 bits contain a palette index. Only the <name|X11> port
  calls <cpp|initialize_colors> and could use it; with <name|Qt> the basic
  colors are initialized statically and true colors are always used.

  The file also defines global colors <cpp|black>, <cpp|white>,
  <cpp|red>, ..., <cpp|light_grey>, <cpp|grey>, <cpp|dark_grey> and
  <cpp|tm_background>, which are used throughout the kernel for interface
  elements, and the value <cpp|pastel> (223) used for the pastel colors of
  the <TeXmacs> dictionary.

  <section|Named colors>

  Colors in documents are strings: the value of <src-var|color>,
  <src-var|bg-color>, <src-var|fill-color> and similar variables, the color
  arguments of <markup|with>, of pencils and brushes and of graphical
  objects. They are converted by

  <\cpp-code>
    color named_color (string s, int a= 255);
  </cpp-code>

  which accepts, after converting <cpp|s> to lower case,

  <\itemize>
    <item>hexadecimal notations <verbatim|#rgb>, <verbatim|#rgba>,
    <verbatim|#rrggbb> and <verbatim|#rrggbbaa>;

    <item>names from five dictionaries, looked up in this order: the
    <TeXmacs> colors (<verbatim|tm_colors.hpp>: <verbatim|red>,
    <verbatim|dark red>, <verbatim|pastel blue>, <verbatim|broken white>,
    ...), the <name|X11> colors (<verbatim|x11_colors.hpp>, including
    <verbatim|gray0> to <verbatim|gray100> and spellings with and without
    spaces), the <name|SVG>/<name|HTML> colors, the <LaTeX> <name|xcolor>
    base colors and the <name|dvips> colors (given in <abbr|CMYK> and
    converted by <cpp|cmyk_color>).
  </itemize>

  Because of the order, a name defined in several dictionaries takes its
  <TeXmacs> value: <verbatim|orange> is <verbatim|#ff8000>, not the
  <name|SVG> <verbatim|#ffa500>. The dictionaries are filled into hash
  tables on the first lookup. Unknown names yield <em|black>; to tell an
  unknown name from black, use <cpp|is_color_name>, which treats a result
  with zero <abbr|RGB> components as valid only for <verbatim|"black"> and
  strings starting with <verbatim|"#000">.

  The second argument multiplies the alpha of the color: the typesetter
  passes the current opacity, <cpp|edit_env_rep::alpha>, which is decoded
  from <src-var|opacity> (<verbatim|Typeset/Env/env_semantics.cpp>), so
  <cpp|edit_env_rep::get_color (var)> returns a color already combined with
  the opacity. Pencils and brushes (<verbatim|Graphics/Renderer/pencil.cpp>,
  <verbatim|brush.cpp>) call <cpp|named_color> for atomic color values and
  build pattern brushes for compound ones.

  The converse operations are <cpp|get_hex_color> (which drops the alpha
  byte when it is 255), <cpp|named_rgb_color> and
  <cpp|get_named_rgb_color>. <cpp|named_color_to_xcolormap> returns the
  <name|xcolor> option (<verbatim|"xcolor">, <verbatim|"x11names">,
  <verbatim|"svgnames">, <verbatim|"dvipsnames"> or <verbatim|"texmacs">)
  which defines a color name; the <LaTeX> converter uses it to declare the
  right color model (<verbatim|convert/latex/tmtex.scm>). <cpp|xpm_color>
  is a separate parser for the colors of <name|XPM> images (only
  <verbatim|#rgb>, <verbatim|#rrggbb>, <verbatim|#rrrrggggbbbb>,
  <verbatim|none> and <name|X11> names), used when loading <name|XPM> icons
  (<verbatim|Graphics/Pictures/picture.cpp>).

  <section|Color primitives and <scheme> access>

  Three primitives compute colors in documents
  (<verbatim|Typeset/Env/env_exec.cpp>); all of them return hexadecimal
  strings:

  <\description>
    <item*|<markup|rgb-color>>(<cpp|exec_rgb_color>) builds a color from
    red, green, blue and optionally alpha components.

    <item*|<markup|rgb-access>>(<cpp|exec_rgb_access>) extracts one
    component of a color, selected by <verbatim|r>, <verbatim|g>,
    <verbatim|b>, <verbatim|a> or <verbatim|0> to <verbatim|3>.

    <item*|<markup|blend>>(<cpp|exec_blend>) composes the first color over
    the second one with <cpp|blend_colors>.
  </description>

  Animations interpolate colors componentwise in <cpp|morph_color>
  (<verbatim|Typeset/Env/env_animate.cpp>).

  From <scheme>, the glue (<verbatim|Scheme/Glue/build-glue-basic.scm>)
  exports <scm|(color <scm-arg|name>)> (<cpp|named_color>, returning the
  color word as an integer), <scm|get-hex-color>,
  <scm|named-color-\<gtr\>xcolormap>, <scm|rgba-\<gtr\>named-color> and
  <scm|named-color-\<gtr\>rgba>.

  <section|Reverse colors>

  The option <verbatim|-r> (<verbatim|-reverse>) of the command line calls
  <cpp|set_reverse_colors (true)>: the whole display is shown in a
  \Pdark\Q variant in which each color is replaced by
  <cpp|reverse (r, g, b)>. This function keeps the hue, replaces the mean
  level <math|t> of the three components by <math|255-t>, and rescales the
  deviations from the mean so that the relative saturation is preserved;
  black becomes white and saturated red becomes <verbatim|#ff8080>.

  The reversal is applied in two places, and both are needed to understand
  the values one sees:

  <\enumerate>
    <item><cpp|rgb_color> reverses its arguments before packing them, and
    <cpp|get_rgb_color> reverses again when unpacking. The color word
    therefore holds the <em|displayed> color, while the components seen by
    the program are (approximately) the logical ones.

    <item>The <name|Qt> conversions <cpp|to_qcolor> and <cpp|to_color>
    (<verbatim|Plugins/Qt/qt_utilities.cpp>) and the glyph and picture
    drawing code (<verbatim|qt_renderer.cpp>, <verbatim|qt_picture.cpp>)
    reverse the logical components once more before handing them to
    <name|Qt>.
  </enumerate>

  Since <cpp|reverse> is not exactly an involution, components read back
  through <cpp|get_rgb_color> may differ from those passed to
  <cpp|rgb_color>: in reverse mode, <cpp|rgb_color (255, 0, 0)> reads back
  as <math|(255,1,1)>.

  <section|Floating point colors>

  <cpp|true_color> (<verbatim|Graphics/Colors/true_color.hpp>) stores the
  four components as <cpp|double>s between 0 and 1, in the fields
  <cpp|r>, <cpp|g>, <cpp|b> and <cpp|a>. It converts implicitly from and to
  <cpp|color> (with rounding), and is the pixel type of the raster pictures
  on which the effects operate (see <hlink|raster pictures and
  effects|images-pictures.en.tm>). It provides:

  <\itemize>
    <item>componentwise arithmetic (<cpp|+>, <cpp|->, <cpp|*>, <cpp|/>,
    scalar multiplication), <cpp|min>, <cpp|max>, <cpp|normalize>
    (clamping to <math|[0,1]>) and <cpp|hypot>;

    <item>alpha handling: <cpp|mul_alpha> and <cpp|div_alpha>
    (premultiplied form and back), <cpp|apply_alpha>, <cpp|copy_alpha>,
    <cpp|clear_alpha>;

    <item>composition operators <cpp|source_over> (the second color over
    the first one), <cpp|towards_source> and <cpp|alpha_distance>, used by
    the composition effects;

    <item>weighted mixtures <cpp|mix> of two or four colors, in
    premultiplied form (<verbatim|true_color.cpp>);

    <item>color transformations returned as
    <cpp|unary_function\<less\>true_color,true_color\<gtr\>>:
    <cpp|color_matrix_function> (a <math|4\<times\>5> matrix acting on
    <math|(r,g,b,a,1)>, used by <markup|eff-color-matrix>),
    <cpp|make_transparent_function> and <cpp|make_opaque_function>.
  </itemize>

  The three dimensional objects also use <cpp|true_color> to compute
  lighting (see <hlink|the algebra library and three dimensional
  objects|geometry-algebra.en.tm>).

  <section|Pitfalls>

  <\itemize>
    <item><markup|rgb-color> with three arguments makes <TeXmacs> abort,
    and with four arguments ignores the alpha component. The test in
    <cpp|exec_rgb_color> is inverted
    (<verbatim|Typeset/Env/env_exec.cpp:1904>):

    <\cpp-code>
      tree t4= (N(t)==4? tree ("255"): exec (t[3]));
    </cpp-code>

    so with three arguments the missing fourth child is read. Verified at
    run time: <verbatim|(rgb-color "255" "0" "0" "128")> gives
    <verbatim|#FF0000>, and <verbatim|(rgb-color "255" "0" "0")> terminates
    the program with an uncaught exception.

    <item><cpp|blend_colors> computes the resulting alpha as
    <math|(b<rsub|A>(255-f<rsub|A>)+f<rsub|A><rsup|2>)/255> instead of
    <math|(b<rsub|A>(255-f<rsub|A>)+255f<rsub|A>)/255>
    (<verbatim|Graphics/Colors/colors.cpp:159>), so blending a translucent
    color over an opaque one gives a translucent result. Verified at run
    time: <verbatim|(blend "#ff000080" "#0000ff")> gives
    <verbatim|#80007FBF> instead of an opaque <verbatim|#80007F>.

    <item><cpp|reverse (r, g, b)> uses the integer mean <math|t> and an
    unbounded saturation factor (<verbatim|colors.cpp:119-136>). For nearly
    grey colors, the rounding error of <math|t> is amplified: in reverse
    mode <verbatim|#fefefd> (almost white) is displayed as
    <math|(129,129,2)>, a dark yellow, instead of almost black.

    <item><cpp|max (true_color, true_color)> takes the <em|minimum> of the
    green and alpha components (<verbatim|true_color.hpp:130-131>).

    <item><cpp|operator *= (true_color&, const true_color&)> prints both
    operands on the standard output each time it is called
    (<verbatim|true_color.hpp:96>).

    <item>Unknown color names silently become black, which also makes
    <cpp|is_color_name> the only reliable way to validate user input.

    <item>The special syntax <verbatim|gray<em|n>> in <cpp|color_from_name>
    is dead code: the test <verbatim|s (1,4) == "gray"> compares a three
    character substring with a four character string
    (<verbatim|colors.cpp:353>). The names still work because the
    <name|X11> dictionary defines <verbatim|gray0> to <verbatim|gray100>.

    <item><verbatim|colors.hpp> declares <cpp|get_xpm_color>,
    <cpp|get_cmyk_color> and the variable <cpp|reverse_color>, none of
    which is defined, and defines the five dictionary hash tables as
    <cpp|static>, so that every file including the header gets its own
    empty copies; the accessors <cpp|x11_color>, <cpp|svg_color>, ...
    only work inside <verbatim|colors.cpp>. <verbatim|named_colors.hpp>
    contains conflicting stub definitions returning black and is not
    included anywhere.
  </itemize>

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
