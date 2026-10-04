<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Virtual fonts and the vfn language>

  <section|Principle>

  A virtual font provides glyphs which are <em|computed> from the glyphs of
  a <em|base font>. Each virtual glyph is defined by a small expression,
  for instance

  <\scm-code>
    (longrightarrow (glue minus rightarrow))

    (nleadsto (join (0 0 leadsto) (0 0 Arrownot)))

    (emu-backslash (hor-flip /))
  </scm-code>

  which says that a long arrow is a minus sign glued to an arrow, that a
  negated <verbatim|\<less\>leadsto\<gtr\>> is the superposition of
  <verbatim|\<less\>leadsto\<gtr\>> and of a negation slash, and that a
  backslash can be obtained by mirroring a slash. The base font may itself
  be a smart font, so the components can come from anywhere.

  A virtual font must be able to do two things with such an expression:

  <\itemize>
    <item><em|Compile> it into a bitmap <cpp|glyph> and a <cpp|metric>
    (logical and ink bounding boxes). This is
    <cpp|virtual_font_rep::compile>; it is used for the metrics in all
    cases and for drawing on the screen. Glyph operations are those of
    <verbatim|Graphics/Bitmap_fonts/bitmap_font.hpp> (<cpp|join>,
    <cpp|move>, <cpp|hor_flip>, <cpp|stretched>, <cpp|clip>, ...).

    <item><em|Draw> it directly on a renderer, using the vector drawing of
    the components and the transformations of the renderer
    (<cpp|virtual_font_rep::draw_tree>). This is used for printing and
    <abbr|PDF> export, so that virtual symbols remain vector graphics.
    Operations which can only be done on bitmaps (pixel intersection, flood
    filling, ...) force the bitmap path.
  </itemize>

  <section|Loading virtual fonts>

  <subsection|The <verbatim|.vfn> files>

  Virtual font definitions are stored in
  <verbatim|$TEXMACS_PATH/fonts/virtual> (and may be overridden in
  <verbatim|$TEXMACS_HOME_PATH/fonts/virtual>). A file
  <verbatim|<em|name>.vfn> contains one <scheme> expression

  <\scm-code>
    (virtual-font

    \ \ (name-1 definition-1)

    \ \ (name-2 definition-2)

    \ \ ...)
  </scm-code>

  The standard files are:

  <\description-paragraphs>
    <item*|<verbatim|tradi-long.vfn>, <verbatim|tradi-negate.vfn>,
    <verbatim|tradi-misc.vfn>>Traditional constructions: long arrows,
    negated relations, dots, flipped letters, ... Used by the old
    <TeX> based math fonts and as the last fallback of smart fonts.

    <item*|<verbatim|emu-fundamental.vfn>>Basic building blocks for the
    emulation of symbols in arbitrary text fonts: a good minus sign, slashes
    of various sizes for negations, arrow heads and arrow bars,
    <verbatim|sim>, <verbatim|approx>, <verbatim|equiv>, centered dots, ...
    All other <verbatim|emu-*> fonts are stacked on top of it.

    <item*|<verbatim|emu-greek.vfn>, <verbatim|emu-operators.vfn>,
    <verbatim|emu-relations.vfn>, <verbatim|emu-orderings.vfn>,
    <verbatim|emu-setrels.vfn>, <verbatim|emu-arrows.vfn>>Emulation of
    Greek capitals, binary operators, relations, orderings, set relations
    and arrows from the glyphs of a text font.

    <item*|<verbatim|emu-bracket.vfn>>Brackets: angle brackets built from
    slashes, floors and ceilings from square brackets, double and triple
    brackets and bars (names <verbatim|emu-...>).

    <item*|<verbatim|emu-large.vfn>, <verbatim|emu-alt-large.vfn>>Big
    operators (<verbatim|big-iint-1>, <verbatim|big-oint-2>, ...) and
    parameterized extensible delimiters (<verbatim|rubber-lparenthesis-#>,
    ...) for <cpp|poor_rubber_font> and <cpp|rubber_assemble_font>.
  </description-paragraphs>

  <subsection|Translators>

  Files are loaded by <cpp|load_virtual> (<verbatim|Graphics/Fonts/translator.cpp>),
  usually through <cpp|load_translator>, which first looks for an encoding
  file <verbatim|<em|name>.enc> in <verbatim|fonts/enc> and otherwise
  loads the virtual font. Both produce a <cpp|translator>:

  <\cpp-code>
    struct translator_rep: rep\<less\>translator\<gtr\> {

    \ \ int\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ cur_c;

    \ \ hashmap\<less\>string,int\<gtr\>\ \ dict;\ \ \ \ \ \ // symbol -\<gtr\> code

    \ \ array\<less\>tree\<gtr\>\ \ \ \ \ \ \ \ \ \ virt_def;\ \ // code -\<gtr\> definition (virtual fonts)

    };
  </cpp-code>

  For a virtual font, the code of an entry is its position in the file
  (starting at 1). Names of more than one character are stored in the
  dictionary with angle brackets, so the entry <verbatim|longrightarrow>
  defines the symbol <verbatim|\<less\>longrightarrow\<gtr\>>. Translators
  are resources: each file is parsed only once per session.

  <subsection|The <cpp|virtual_font_rep> class>

  <\explain>
    <cpp|font virtual_font (font base, string name, int size, int hdpi, int
    vdpi, bool extend)><explain-synopsis|construct a virtual font>
  <|explain>
    Builds the virtual font <src-arg|name> on top of <src-arg|base>. Its
    resource name is the base name followed by
    <verbatim|#virtual-<em|name><em|size>@<em|hdpi>> (or
    <verbatim|#enhance-...> in extend mode, with
    <verbatim|x<em|vdpi>> if the resolutions differ). The
    <src-arg|extend> flag selects between two modes:

    <\itemize>
      <item><em|Plain mode> (<cpp|extend == false>): the font only provides
      the symbols of the <verbatim|.vfn> file. Atomic names inside the
      definitions always refer to the base font. This is how smart fonts
      use <verbatim|tradi-*> and <verbatim|emu-bracket>, with the smart
      font itself as base (<verbatim|("virtual" name)> subfonts).

      <item><em|Extend mode> (<cpp|extend == true>): the font provides all
      symbols of the base font, plus the virtual symbols which the base font
      does not have. Atomic names inside definitions refer to the base font
      if it supports them, and otherwise to the other definitions of the
      same file, recursively. This is how the <verbatim|emu-*> fonts are
      used: <cpp|virtual_font (virtual_font (main, "emu-fundamental", ...,
      true), "emu-arrows", ..., true)>.
    </itemize>
  </explain>

  The class keeps the base font, the translator <cpp|virt>, the resolution,
  a <cpp|font_metric> and <cpp|font_glyphs> table indexed by code (filled
  lazily), and caches <cpp|trm> (metrics of compiled subexpressions) and
  <cpp|sup_bit>, <cpp|sup_svg> (results of the support tests).

  <subsection|Units>

  Numbers in definitions are expressed in units of the font size: the
  horizontal unit <cpp|hunit= ((size*hdpi)/72)*PIXEL> and the vertical unit
  <cpp|vunit> (same with <cpp|vdpi>) correspond to one em of the base font.
  Thus <verbatim|(0.3 0 .)> is a period moved 0.3 em to the right. Some
  operators instead use fractions of the bounding box of a glyph; this is
  indicated below.

  <section|Symbols and lookup>

  <subsection|Naming components>

  Inside a definition, an atom denotes a glyph:

  <\itemize>
    <item>An atom of one character denotes that character: <verbatim|/>,
    <verbatim|=>, <verbatim|[>, <verbatim|.>, <verbatim|+>.

    <item>A longer atom <verbatim|foo> denotes the universal symbol
    <verbatim|\<less\>foo\<gtr\>>: <verbatim|minus>,
    <verbatim|rightarrow>, <verbatim|less> (for <verbatim|\<less\>>),
    <verbatim|#2039> (for the <name|Unicode> character
    <verbatim|\<less\>#2039\<gtr\>>).

    <item>The parentheses, which cannot be written as atoms, are
    <verbatim|#28> and <verbatim|#29>.
  </itemize>

  <subsection|Parameterized symbols>

  An entry whose name ends with <verbatim|-#> defines a family of symbols.
  A request for <verbatim|\<less\>rubber-lparenthesis-7\<gtr\>> is decoded
  by <cpp|decode_sharp> into the entry
  <verbatim|\<less\>rubber-lparenthesis-#\<gtr\>> and the parameter
  <verbatim|7>, and every atom <verbatim|#> in the definition is replaced
  by the parameter (<cpp|subst_sharp>; atoms like <verbatim|#239C> which
  start with a hexadecimal digit are left alone). For example, from
  <verbatim|emu-large.vfn> and <verbatim|emu-alt-large.vfn>:

  <\scm-code>
    (rubber-lparenthesis-# (ver-extend #28 0.5 # 0.1))

    \;

    (rubber-lparenthesis-#

    \ \ (glue-above #239D

    \ \ \ \ (glue-above (ver-take #239C 0.5 # 0.25)

    \ \ \ \ \ \ #239B)))
  </scm-code>

  The first one stretches an ordinary parenthesis at half its height by
  <math|0.1\<times\>n> em; the second one assembles the <name|Unicode>
  parenthesis pieces <verbatim|U+239B>\U<verbatim|U+239D> with a variable
  number of copies of the extension piece.

  <cpp|virtually_defined (c, name)> tells whether a symbol is defined in a
  virtual font file, taking parameterized symbols into account.

  <subsection|Getting a glyph>

  <cpp|virtual_font_rep::get_char (s, fnm, fng)> returns the code of
  <src-arg|s> together with the metric and glyph tables to be used,
  compiling the definition on first use:

  <\itemize>
    <item>For <verbatim|\<less\>name\<gtr\>>, the code is
    <cpp|virt-\<gtr\>dict[s]>, after decoding of a numeric parameter if
    needed. The glyph is stored in the global tables of the font.

    <item>A string of <em|one byte> is interpreted as a code, i.e. as the
    position of the entry in the file. This is the convention of the old
    <cpp|math_font>, whose translators map symbols to codes.

    <item>A string consisting of a code byte followed by a parameter
    (another legacy convention, used by <cpp|poor_rubber_font> and
    <cpp|math_font>) creates a separate one-glyph table for this
    parameter value.
  </itemize>

  <section|The vfn language>

  The following reference lists all constructs recognized by
  <cpp|virtual_font_rep::compile_bis> (bitmaps and metrics),
  <cpp|virtual_font_rep::draw_tree> (vector drawing) and
  <cpp|virtual_font_rep::supported>. In the descriptions, <math|g>,
  <math|g<rsub|1>>, <math|g<rsub|2>> denote glyph expressions,
  <math|r> a <em|reference> glyph expression, and numeric arguments may
  be written <verbatim|*> to mean \Pdefault\Q or \Pdo not change\Q.
  Operators marked <em|bitmap only> are not drawn as vector graphics: a
  glyph which uses them is always rendered as a bitmap.

  <subsection|Selection and variables>

  <\description>
    <item*|<verbatim|(<em|dx> <em|dy> <em|g>)>>The glyph <math|g> moved by
    <math|(dx,dy)> em.

    <item*|<verbatim|(with <em|var> <em|expr> <em|g>)>>Evaluate the numeric
    expression <math|expr> (see below) and substitute the result for every
    atom <math|var> in <math|g>.

    <item*|<verbatim|(or <em|g<rsub|1>> ... <em|g<rsub|n>>)>>The first of
    <math|g<rsub|1>,\<ldots\>,g<rsub|n-1>> which is supported (all its
    components exist), and <math|g<rsub|n>> otherwise.

    <item*|<verbatim|(font <em|g> <em|fam<rsub|1>> ...)>>The glyph <math|g>,
    but only supported if the base font is a <name|Unicode> font of one of
    the given families (its name contains
    <verbatim|unicode:<em|fam>>). Used inside <verbatim|or> to apply
    font specific constructions.

    <item*|<verbatim|(italic <em|g> <em|slope> <em|corr>)>>The glyph
    <math|g>, with the given slope (returned by <cpp|get_left_slope> and
    <cpp|get_right_slope>) and right italic correction in em.
  </description>

  <subsection|Combination>

  <\description-paragraphs>
    <item*|<verbatim|(join <em|g<rsub|1>> ... <em|g<rsub|n>>)>>Superposition
    with a common origin; the bounding boxes are merged.

    <item*|<verbatim|(add <em|g<rsub|1>> <em|g<rsub|2>>)>>Superposition, with
    <math|g<rsub|2>> horizontally centered on <math|g<rsub|1>>.

    <item*|<verbatim|(glue <em|g<rsub|1>> <em|g<rsub|2>>)>>Horizontal
    concatenation with a small overlap of <math|1.75> points (in units of
    <cpp|wpt>), suitable for joining strokes.
    <verbatim|(glue* <em|g<rsub|1>> <em|g<rsub|2>> [<em|d>])> concatenates
    without overlap; the optional <math|d> shifts <math|g<rsub|2>> further
    by <math|d> times the size unit of the font (negative for an
    overlap).

    <item*|<verbatim|(row <em|g<rsub|1>> <em|g<rsub|2>>)>>Concatenation of
    the ink of <math|g<rsub|1>> and <math|g<rsub|2>> with a slight overlap,
    centered in the logical width of the glued glyphs.

    <item*|<verbatim|(glue-above <em|g<rsub|1>> <em|g<rsub|2>>
    [<em|sep>])>, <verbatim|(glue-below ...)>><math|g<rsub|2>> on top of
    (resp. below) <math|g<rsub|1>>, with an optional extra separation in em.

    <item*|<verbatim|(stack <em|g<rsub|1>> <em|g<rsub|2>> [<em|sep>])>><math|g<rsub|2>>
    below <math|g<rsub|1>>, the whole being raised by half the height of
    <math|g<rsub|2>>.

    <item*|<verbatim|(stack-equal <em|g<rsub|1>> <em|g<rsub|2>>)>,
    <verbatim|(stack-less ...)>>A stack with the separation between the
    bars of <verbatim|=>, vertically centered on <verbatim|=> resp.
    <verbatim|\<less\>>.

    <item*|<verbatim|(right-fit <em|g<rsub|1>> <em|g<rsub|2>>
    <em|f>)>, <verbatim|(left-fit ...)>><math|g<rsub|2>> moved as close as
    possible to the right (left) of <math|g<rsub|1>> without collision
    (<cpp|collision_offset>); the logical width is extended by
    <math|f> times the width of <math|g<rsub|2>>.

    <item*|<verbatim|(intersect <em|g<rsub|1>> <em|g<rsub|2>>)>,
    <verbatim|(exclude <em|g<rsub|1>> <em|g<rsub|2>>)>>Pixelwise intersection
    and difference. Bitmap only.

    <item*|<verbatim|(bar-right <em|g<rsub|1>> <em|g<rsub|2>>)>,
    <verbatim|bar-left>, <verbatim|bar-top>, <verbatim|bar-bottom>>Attach the
    bar <math|g<rsub|2>> to the side of <math|g<rsub|1>> (the starred forms
    <verbatim|bar-right*>, <verbatim|bar-bottom*> do not align first).
    Bitmap only.

    <item*|<verbatim|(negate <em|g> <em|bar>)>>The negation of a relation:
    <math|bar> is centered on <math|g>, vertically magnified if its height
    does not fit, and the logical box of <math|g> is kept.

    <item*|<verbatim|(reslash <em|g> <em|r>)>>The slash <math|g>,
    transformed to fit the box of <math|r>.
  </description-paragraphs>

  <subsection|Geometric transformations>

  <\description-paragraphs>
    <item*|<verbatim|(magnify <em|g> <em|mx> <em|my>)>>Scale by the given
    factors.

    <item*|<verbatim|(deepen <em|g> <em|my> <em|pen>)>,
    <verbatim|(widen <em|g> <em|mx> <em|pen>)>>Vertical (horizontal)
    stretching which preserves strokes of width <math|pen> em.

    <item*|<verbatim|(hor-flip <em|g>)>, <verbatim|(ver-flip
    <em|g>)>>Mirror images.

    <item*|<verbatim|(rot-left <em|g>)>, <verbatim|(rot-right <em|g>)>>Rotation
    by <math|\<pm\>90> degrees. <verbatim|(rotate <em|g> <em|angle>
    [<em|xf> <em|yf>])> rotates by an arbitrary angle (in radians) around a
    point given as fractions of the logical box.

    <item*|<verbatim|(scale <em|g> <em|r> <em|sx> <em|sy>)>,
    <verbatim|scale*>>Scale <math|g> so that its logical (for
    <verbatim|scale*>: ink) width and height become those of <math|r>;
    the exponents <math|sx>, <math|sy> interpolate between no scaling
    (<verbatim|0> or <verbatim|*>) and full scaling (<verbatim|1>).
    The variants <verbatim|fscale> and <verbatim|fscale*> (bitmap only)
    take a fifth argument, a pen width, and use <cpp|widen> and
    <cpp|deepen> instead of a plain stretching.

    <item*|<verbatim|(hor-scale <em|g> <em|r>)>>Scale the horizontal ink of
    <math|g> to the one of <math|r> and center it on <math|r>.

    <item*|<verbatim|(hor-extend <em|g> <em|pos> <em|n> [<em|f>])>>Make
    <math|g> wider by <math|n> em (or <math|n\<times\>f> em), by repeating
    the column at the fraction <math|pos> of its width.
    <verbatim|ver-extend> does the same vertically, and
    <verbatim|(ver-take <em|g> <em|pos> <em|n> [<em|f>])> only keeps the
    repeated part. <verbatim|(hor-take <em|g> <em|pos> <em|n> [<em|f>])> is
    the horizontal counterpart of <verbatim|ver-take>: a piece of width
    <math|n> (or <math|n\<times\>f>) times the logical width of <math|g>,
    made of the column at the fraction <math|pos> of its width; on printers
    it is drawn as vectors, by repeating narrow clipped copies of the
    glyph.

    <item*|<verbatim|(curly <em|g>)>, <verbatim|(unserif <em|g> [<em|c>])>,
    <verbatim|(bottom-edge <em|g> [<em|penh> <em|keepy>])>,
    <verbatim|(flood-fill <em|g> <em|xf> <em|yf>)>,
    <verbatim|(junc-left <em|g> <em|w>)>, <verbatim|(junc-right ...)>,
    <verbatim|(circle <em|r> <em|w>)>>Special bitmap manipulations:
    making a stroke curly, removing serifs (of the character <math|c>),
    keeping the bottom edge, filling a closed contour from a point given as
    fractions of the ink box, junctions, and a circle of radius <math|r>
    and pen width <math|w> em. Bitmap only.

    <item*|<verbatim|(bitmap <em|g>)>>The glyph <math|g>, but always
    rendered as a bitmap.

    <item*|<verbatim|(copy <em|g>)>>A copy of the glyph.
  </description-paragraphs>

  <subsection|Positioning and bounding boxes>

  <\description-paragraphs>
    <item*|<verbatim|(align <em|g> <em|r> <em|xa> <em|ya> [<em|xa<rsub|2>>
    <em|ya<rsub|2>>])>>Move <math|g> so that the point at the fractions
    <math|(xa,ya)> of its logical box coincides with the point
    <math|(xa<rsub|2>,ya<rsub|2>)> (default: the same fractions) of the
    logical box of <math|r>. A <verbatim|*> means \Pno move in this
    direction\Q. <verbatim|align*> does the same using the ink boxes.

    <item*|<verbatim|(crop <em|g>)>, <verbatim|hor-crop>,
    <verbatim|ver-crop>, <verbatim|left-crop>, <verbatim|right-crop>,
    <verbatim|top-crop>, <verbatim|bottom-crop>>Replace (parts of) the
    logical box by the ink box.

    <item*|<verbatim|(enlarge <em|g> <em|l> <em|r> <em|b> <em|t>)>>Enlarge
    the logical box by the given amounts in em (trailing arguments may be
    omitted).

    <item*|<verbatim|(unindent <em|g>)>, <verbatim|(unindent* <em|g>)>>Move
    horizontally so that the left (right) side of the logical box is at
    zero.

    <item*|<verbatim|(clip <em|g> <em|x1> <em|x2> <em|y1>
    <em|y2>)>>Clip to the given coordinates in em.

    <item*|<verbatim|(part <em|g> <em|x1> <em|x2> <em|y1> <em|y2>
    [<em|dx> <em|dy>])>>Clip to a part of the logical box given by
    fractions, and optionally move the result by fractions of the box.

    <item*|<verbatim|(pretend <em|g> <em|r>)>,
    <verbatim|hor-pretend>, <verbatim|ver-pretend>,
    <verbatim|left-pretend>, <verbatim|right-pretend>>Draw <math|g> with
    (parts of) the logical box of <math|r>.

    <item*|<verbatim|(min-width <em|g<rsub|1>> <em|g<rsub|2>>)>,
    <verbatim|max-width>, <verbatim|min-height>,
    <verbatim|max-height>>Choose the narrower, wider, lower or higher
    glyph.
  </description-paragraphs>

  <subsection|Numeric expressions>

  The second argument of <verbatim|with> is evaluated by
  <cpp|virtual_font_rep::exec>, which returns numbers as strings (or
  <verbatim|"error">):

  <\description-paragraphs>
    <item*|<verbatim|+>, <verbatim|->, <verbatim|*>, <verbatim|/>,
    <verbatim|min>, <verbatim|max>>Arithmetic (<verbatim|-> and
    <verbatim|/> are binary).

    <item*|<verbatim|(left <em|g>)>, <verbatim|right>, <verbatim|bottom>,
    <verbatim|top>, <verbatim|width>, <verbatim|height>>Coordinates and
    dimensions of the logical box, in em.

    <item*|<verbatim|(xpos <em|g> <em|xf>)>, <verbatim|(ypos <em|g>
    <em|yf>)>, <verbatim|(xpos <em|g> <em|xf> <em|yf> <em|dir>)>>Coordinate
    of a point given by fractions of the box; with four arguments, the
    point is moved in the direction <verbatim|+> or <verbatim|-> until the
    ink boundary (<cpp|probe>).

    <item*|<verbatim|(penw <em|g> <em|x1> <em|x2> <em|y>)>,
    <verbatim|(penh <em|g> <em|x> <em|y1> <em|y2>)>>Measured stroke width
    (between the horizontal fractions <math|x1> and <math|x2>, at height
    <math|y>) or stroke height (at <math|x>, between <math|y1> and
    <math|y2>) of the glyph.

    <item*|<verbatim|(sep-equal)>, <verbatim|(frac-width)>>The distance
    between the bars of <verbatim|=>, and the thickness of a minus sign.
  </description-paragraphs>

  For instance, the <verbatim|approx> symbol of
  <verbatim|emu-fundamental.vfn> superposes two copies of the emulated
  <verbatim|sim>, shifted by the difference between the heights of
  <verbatim|minus> and <verbatim|=>:

  <\scm-code>
    (sim (align ~ = 0 0.5))

    (approx (with y (- (height minus) (height =))

    \ \ \ \ \ \ \ \ \ \ (align (join sim (0 y sim)) = * 0.5)))
  </scm-code>

  <section|Rendering virtual glyphs>

  <subsection|Metrics and glyphs>

  <cpp|compile (t, ex)> first consults the caches: the metric of each
  compiled subexpression is stored in <cpp|trm>, but the glyph itself is
  only cached in <cpp|trg> for the expensive <verbatim|curly>
  construction. The glyphs of whole symbols are stored in the
  <cpp|font_glyphs> table of the font by <cpp|get_char>. For an atom, the
  glyph comes from <cpp|base_fn-\<gtr\>get_glyph> and the metric from
  <cpp|base_fn-\<gtr\>get_extents>, unless (in extend mode) the atom is a
  virtual symbol of the same file. Consequently the base font must
  implement <cpp|get_glyph>; <name|Unicode> fonts rasterize their outlines
  with <name|FreeType>.

  <subsection|Drawing>

  <\cpp-code>
    void

    virtual_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {

    \ \ if (extend && base_fn-\<gtr\>supports (s))

    \ \ \ \ base_fn-\<gtr\>draw_fixed (ren, s, x, y);

    \ \ else if (ren-\<gtr\>is_screen \|\| !supported (s, true)) {

    \ \ \ \ font_metric cfnm;

    \ \ \ \ font_glyphs cfng;

    \ \ \ \ int c= get_char (s, cfnm, cfng);

    \ \ \ \ if (c != -1) ren-\<gtr\>draw (c, cfng, x, y);

    \ \ }

    \ \ else {

    \ \ \ \ tree t= get_tree (s);

    \ \ \ \ if (t != "") draw_tree (ren, t, x, y);

    \ \ }

    }
  </cpp-code>

  On the screen virtual glyphs are drawn as bitmaps through
  <cpp|renderer_rep::draw (int, font_glyphs, SI, SI)>; since the font is
  magnified to the zoom factor before drawing, the bitmaps are computed at
  the screen resolution. On printers, <cpp|supported (s, true)> checks
  that the definition only uses vector-capable operators, and that a
  component taken from a base font which is itself a virtual font is drawn
  as vectors there too; if so,
  <cpp|draw_tree> draws the components with the base font, using
  <cpp|ren-\<gtr\>set_transformation> for <verbatim|magnify>,
  <verbatim|scale>, flips and rotations (through
  <cpp|draw_transformed>) and <cpp|ren-\<gtr\>clip> for <verbatim|clip>,
  <verbatim|part> and the extensions (through <cpp|draw_clipped>, which
  slightly enlarges the clipping box vertically). Otherwise the glyph is
  drawn as a bitmap, which the <abbr|PDF> renderer embeds as a
  <name|Type 3> font.

  <subsection|Other font routines>

  <cpp|supports (s)> is true if (in extend mode) the base font supports
  <src-arg|s> or if the definition of <src-arg|s> exists and all its
  components are supported. In the latter case, <verbatim|or> succeeds as
  soon as one alternative is supported, and <verbatim|font> checks the
  name of the base font. Two routines serve the font inspector:
  <cpp|virtual_font_constructs (fn, s)> tells whether <src-arg|s> is drawn
  by a construction rather than taken from the font which the virtual font
  extends, and <cpp|virtual_font_draws_vectors (fn, s)> whether a
  <name|PDF> export draws it as vectors; the second also decides when a
  smart font prefers <name|STIX Two Math> to an emulation (see <hlink|the
  resolution algorithm|smart-fonts-resolve.en.tm>).
  <cpp|get_xpositions> treats each virtual symbol as
  one block (inner positions are put in the middle), the slope and right correction come from <verbatim|italic>
  annotations, and <cpp|magnify> rebuilds the virtual font on the
  magnified base font.

  <section|Enhancing a font>

  <cpp|virtual_enhance_font (font base, string virt)>
  (<verbatim|Graphics/Fonts/virtual_enhance.cpp>) wraps <src-arg|base> and
  an extend mode virtual font <src-arg|virt> built on it. All symbols
  supported by the base font are rendered by the base font; universal
  symbols <verbatim|\<less\>...\<gtr\>> which it lacks are taken from the
  virtual font. <cpp|poor_rubber_font> uses it to add the
  <verbatim|emu-bracket> symbols to a <name|Unicode> font before building
  extensible delimiters from them.

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
