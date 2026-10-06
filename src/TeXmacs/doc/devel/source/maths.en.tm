<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Mathematical typesetting>

  <section|Introduction>

  In this chapter we describe the algorithms used by <TeXmacs> in order to
  typeset mathematical formulas. This is a difficult subject, because
  esthetics and effectiveness do not always go hand in hand. Until now, <TeX>
  is widely accepted for having achieved an optimal compromise in this
  respect. Nevertheless, we thought that several improvements could still be
  made, which have now been implemented in <TeXmacs>. We will shortly
  describe the motivations behind them.

  In order to obtain esthetic formulas, what criteria should we use? It is
  often stressed that good typesetting allows the reader to concentrate on
  what he reads, without being distracted by ugly typesetting details. Such
  distracting details arise when distinct, though similar parts of text are
  typeset in a non uniform way:

  <\description>
    <item*|Different base lines>The eye expects text of a similar nature to
    be typeset with respect to a same base line. For instance, in
    <math|x+y+z>, the bottoms of the <math|x> and <math|z> should be at the
    same height as the bottom of the <math|u>-part in the <math|y>. This
    should again be the case in <math|2<rsup|x>+2<rsup|y>+2<rsup|z>>.

    <item*|Unequal spacing>Different components of text with approximately
    the same function should be separated by equal amounts of space. For
    instance, in <math|a<rsup|2>+f<rsup|2>>, the typesetter should notice the
    hangover of the <math|f>. This should again be the case in
    <math|e<rsup|a>+e<rsup|f>+e<rsup|x>>. Similarly, the distance between the
    baselines of the <math|a> and the <math|i> in <math|a<rsub|i>> should not
    be disproportionally large with respect to the height of an <math|x>.
  </description>

  Additional difficulties may arise when considering automatically generated
  formulas, in which case line breaking has to be dealt with in a
  satisfactory way.

  Unfortunately, the different esthetic criteria may enter into conflict with
  each other. For instance, consider the formula
  <math|x<rsub|p>+x<rsub|p><rsup|2>>. On the one hand, the baselines of the
  scripts should be the same, but the other hand, the first subscript should
  not be \Pdisproportionally low\Q with respect to the <math|x>.
  Unfortunately, this dilemma can not been solved in a completely
  satisfactory way without the help of a human for the simple reason that the
  computer has no way to know whether the <math|x<rsub|p>> and
  <math|x<rsub|p><rsup|i>> are \Prelated\Q. Indeed, if the <math|x<rsub|p>>
  and <math|x<rsub|p><rsup|i>> are close (like in
  <math|x<rsub|p>+x<rsub|p><rsup|i>>), then it is natural to opt for a common
  base line. However, if they are further away from each other (like in
  <math|x<rsub|p>+<big|sum><rsub|i=0><rsup|\<infty\>>c<rsub|i>x<rsub|p><rsup|i>>),
  then we might want to opt for different base lines and locally optimize the
  rendering of the first <math|x<rsub|p>>.

  Consequently, <TeXmacs> should offer a reasonable compromise for the most
  frequent cases, while offering methods for the user to make finer
  adjustments in the remaining ones. We provide the constructs
  <menu|Format|Transform|Move> and <menu|Format|Transform|Resize> (tags
  <markup|move> and <markup|resize>) to move and resize boxes in order to
  perform such adjustments. For instance, if the
  brackets around the two sums

  <\equation*>
    \<phi\><around*|(|<big|sum><rsub|i>a<rsub|i>x<rsup|i>|)>=\<psi\><around*|(|<big|sum><rsub|<smash|j>>b<rsub|j>y<rsup|j>|)>
  </equation*>

  have different sizes, then one may resize the bottom of the subscript
  <math|j> of the second sum to <verbatim|0fn>. Alternatively, one may resize
  the bottoms of both the <math|i> and <math|j> subscripts to (say)
  <verbatim|-0.3fn>. For easier adjustments you may use
  <menu|Format|Transform|Smash> and <menu|Format|Transform|Inflate> to automatically
  adjust the size of the contents to the height of the character \Px\Q and
  the largest one in the font respectively.

  Notice that one should adjust by preference in a structural and not visual
  way. For instance, one should prefer <verbatim|-0.3fn> to <verbatim|-2mm>
  in the above example, because the second option disallows you to switch to
  another font size for your document. Similarly, you should try not change
  the semantics of the formula. For instance, in the above example, you might
  have added a \Pdummy subscript\Q to the <math|i> subscript of the sum.
  However, this would alter the meaning of the formula (whence make it non
  suitable as input to a computer algebra system) In the future, we plan to
  provide additional constructs in order to facilitate structural adjusting.
  For instance, in the case of a formula like

  <\equation*>
    1+x<rsub|1>+x<rsub|1><rsup|2>+\<cdots\>+x<rsub|2>+x<rsub|1>x<rsub|2>+x<rsub|1><rsup|2>x<rsub|2>+\<cdots\>x<rsub|2><rsup|2>+x<rsub|1>x<rsub|2><rsup|2>+x<rsub|1><rsup|2>x<rsub|2><rsup|2>+\<cdots\>,
  </equation*>

  one might think of a construct to enclose the entire formula into an area,
  where all scripts are forced to be double (using dummy superscripts
  wherever necessary).

  <section|The font parameters>

  Several font parameters are crucial for the correct positioning of the
  different components. They are stored as fields of the class
  <cpp|font_rep> (see <source-link|Graphics/Fonts/font.hpp|src/Graphics/Fonts/font.hpp> and the chapter on
  <hlink|fonts|fonts.en.tm>). The following are often needed:

  <\description>
    <item*|<verbatim|wfn>>The main font reference space <verbatim|1fn>,
    which can be taken as the distance between successive lines of text.

    <item*|<verbatim|y1> and <verbatim|y2>>The bottom and top level for the
    font (for <TeX> fonts, we have <verbatim|y2-y1=wfn>).

    <item*|<verbatim|sep>>The reference minimal space between distinct
    components, like the minimal distance between a subscript and a
    superscript. In fact, <verbatim|sep=wfn/10>.

    <item*|<verbatim|wline>>The width of several types of lines, like the
    fraction and square root bars, wide accents, etc.

    <item*|<verbatim|yfrac>>The height of the fraction bar, which is needed
    for the positioning of fractions and big delimiters. For <TeX> fonts,
    <verbatim|yfrac> is equal to <verbatim|yx/2> below; for <name|Unicode>
    fonts, it is the vertical middle of the minus sign.
  </description>

  The following parameters are mainly needed in order to deal with scripts:

  <\description>
    <item*|<verbatim|yx>>The height of the <math|x> character, which is
    needed for the positioning of scripts. All the remaining parameters are
    actually computed as a function of <verbatim|yx>.

    <item*|<verbatim|ysub_lo_base>>Logical base line for subscripts.

    <item*|<verbatim|ysub_hi_lim>>Subscripts may never physically exceed
    this top height.

    <item*|<verbatim|ysup_lo_base>>Logical base line for superscripts.

    <item*|<verbatim|ysup_lo_lim>>Superscripts may never physically exceed
    this bottom height.

    <item*|<verbatim|ysup_hi_lim>>Suggestion for a physical top line for
    superscripts.

    <item*|<verbatim|yshift>>Possible shift of the base lines when we are
    inside fractions or scripts.
  </description>

  The individual strings in a font also have several important positioning
  properties. First of all, they always admit left and right slopes.
  Furthermore, they admit left and right italic corrections, which are needed
  for the positioning of scripts or when passing from text in upright to text
  in italics (or vice versa). These are computed by the virtual methods
  <cpp|get_left_slope>, <cpp|get_right_slope>, <cpp|get_left_correction>,
  <cpp|get_right_correction> of the font and, at the level of boxes, by the
  methods <cpp|left_slope>, <cpp|right_slope>, <cpp|left_correction>,
  <cpp|right_correction>, <cpp|lsub_correction>, <cpp|rsup_correction>,
  <abbr|etc.> of <cpp|box_rep>. More recent versions of <TeXmacs> also
  implement font-specific microtypographic corrections for the positioning
  of scripts and wide accents (see the files
  <verbatim|Plugins/Freetype/adjust_*.cpp>).

  <section|Some major mathematical constructs>

  <subsection|Fractions>

  The following heuristics are used:

  <\itemize>
    <item>The horizontal middles of the numerator and the denominator are
    taken to be the same.

    <item>The vertical spaces between the numerator <abbr|resp.> denominator
    and the fraction bar is at least <verbatim|sep>.

    <item>The depth (<abbr|resp.> height) of the numerator (<abbr|resp.>
    denominator) is descended (<abbr|resp.> increased) to <verbatim|y1>
    (<abbr|resp.> <verbatim|y2>) if necessary. This forces the base lines of
    not too large numerators <abbr|resp.> denominators to be the same in
    presence of multiple fractions.

    <item>The fraction bar has a overhang of <verbatim|sep/2> to both sides
    and the logical limits of the fraction are another <verbatim|sep/2>
    further. The logical left limit is zero.
  </itemize>

  The italic corrections are not taken into account during the positioning
  algorithms, because this may create the impression that the numerator and
  denominator are not correctly centered with respect to each other.
  Nevertheless, the italic corrections are taken into account in order to
  compute the logical bounding box of the fraction (whose has italic slopes
  vanish at both sides).

  <subsection|Roots>

  The following heuristics are used:

  <\itemize>
    <item>The vertical space between the main argument and the upper bar is
    at least <verbatim|sep>.

    <item>The root itself is typeset like a large delimiter. The positioning
    of a potential script (the index of the root) depends on the font, with
    special cases for some math fonts (see <cpp|sqrt_box_rep> in
    <source-link|math_boxes.cpp|src/Typeset/Boxes/Composite/math_boxes.cpp>).

    <item>The upper bar has a overhang of <verbatim|sep/2> at the right and
    the logical right limit of the root is situated another <verbatim|sep/2>
    further to the right.
  </itemize>

  We take the logical right border plus the italic correction of the main
  argument in order to determine the right hand limit of the upper bar. The
  left italic correction is not needed.

  <subsection|Negations>

  The following heuristics are used:

  <\itemize>
    <item>The negation bar passes through the logical center of the argument.

    <item>The italic corrections of the argument are only taken into account
    during the computation of the logical limits of the negation box (which
    has zero left and right slopes).
  </itemize>

  <subsection|Wide boxes>

  The following heuristics are used:

  <\itemize>
    <item>We use the accents of the font for small accents and
    extensible glyphs or an <with|font-shape|italic|ad hoc> algorithm for
    the wider ones.

    <item>The distance between the main argument and the accent is at least
    <verbatim|sep> (or a distance which depends on the font for small
    accents).

    <item>The accent is positioned horizontally according to the right slope
    of the main argument, with an additional font-specific correction
    (<cpp|wide_correction>).

    <item>The slopes for the accented box are inherited from those of the
    main argument and the italic corrections are adjusted accordingly.

    <item>All script height parameters of the accented box are inherited from
    the main argument. The only exception is <verbatim|ysup_hi_lim>, which
    may be increased by the height of the accent, or determined in the
    generic way, whichever leads to the least value. It is indeed better to
    keep superscripts positioned reasonably low, whenever possible.
  </itemize>

  <section|Subscripts and superscripts>

  The positioning of subscripts and superscripts is a complicated affair, due
  to the conflict between locally and globally optimal esthetics mentioned
  above. The base line for a subscript is determined as follows:

  <\enumerate>
    <item>Always pretend that the subscript has height at least
    <verbatim|y2-yshift> in the script font (actually we should use the
    height of an <math|M> instead).

    <item>Try to position the script at the base line given by the main
    argument.

    <item>If the top limit (given by the main argument) is physically
    exceeded by the subscript, then the base line is moved further down
    accordingly.
  </enumerate>

  The base line for a superscript is determined as follows:

  <\enumerate>
    <item>Try to physically position the superscript beneath the suggested
    top line. Usually, this will place the superscript to far down.

    <item>Move the superscript up to the logical base line if necessary. This
    will usually occur: most of the time, the logical base line is the just
    the height of an <math|x>-script below the suggested top line.

    <item>If the superscript physically descends below the physical under
    limit given by the main box, then we move the superscript further
    upwards.
  </enumerate>

  If both a subscript and a superscript were present, then we still have to
  adjust the base lines: if the top of the subscript and the bottom of the
  superscript are not physically separated by <verbatim|sep>, then we both
  move the subscript and the superscript by the same amount away from each
  other. Because of step 1 in the positioning of the subscript, the base
  lines of double scripts will usually be the same in formulas with several
  of them.

  The right slope and italic correction of a script box may be non trivial.
  In order to compute them, we first determine the script (or main argument),
  whose right limit (taking into account its italic correction) is furthest
  to the right (this may be the main box, in the case of a big integral with
  a tiny subscript). Then the right slope of the main box is inherited by the
  right slope of this script (or main argument). As to the italic correction,
  it is precisely the difference between the right offset of the script plus
  its italic correction minus the logical right coordinate of the entire box.
  The italic correction should be at least zero though. The left slope and
  italic correction are computed in a similar way.

  <section|Big delimiters>

  The automatic positioning and computation of sizes of big delimiters is
  again complicated because of potential conflicts between locally and
  globally optimal esthetics.

  First of all, fonts come only with a discrete set of possible sizes for
  large delimiters (see the section on string encodings in the chapter on
  <hlink|fonts|fonts.en.tm>), before the delimiters are assembled from
  pieces. This is an advantage from the point of view that it favorites
  delimiters around slightly different expressions to have the same
  baselines. However, it has the disadvantage that delimiters are easily
  made \Pone size too large\Q. For this reason, we actually diminish the
  height and the depth of the delimited expression by the small amount
  <verbatim|sep/2>, before computing the sizes of the delimiters.

  Secondly, it is best when the vertical middles of big delimiters occur at
  the height of fraction bars. However, in a formula like

  <\equation*>
    f<around*|(|<frac|1|1+<frac|1|1+<frac|1|1+<frac|1|x>>>>|)>,
  </equation*>

  it may be worth it to descend the delimiters a bit. On the other hand,
  slight vertical shifts in the middles of the delimiters potentially have a
  bad effect on base lines, like in

  <\equation*>
    f<around*|(|<big|sum><rsub|i=1><rsup|b>X<rsub|i>|)>+g<around*|(|<big|sum><rsub|j=1><rsup|a>Y<rsub|j>|)>.
  </equation*>

  In <TeXmacs>, we use the following compromise: we start with the middle of
  the delimited expression as a first approximation to the middle of the
  delimiters. We then try to make the delimiters symmetric with respect to
  the middle of a delimiter of normal size (which is close to the height of
  fraction bars), by extending them at the top or at the bottom, but the
  corresponding shift of the middle may not exceed <verbatim|2 sep>. This
  algorithm is implemented in <cpp|concater_rep::handle_matching> in
  <source-link|Typeset/Concat/concat_post.cpp|src/Typeset/Concat/concat_post.cpp>.

  From a horizontal point of view, we finally have to notice that we adapted
  the metrics of the big delimiters in a way that potential scripts are
  positioned in a better way. For instance, according to the <TeX>
  <verbatim|tfm> file, in a formula like

  <\equation*>
    <around*|(|A+<around*|(|<big|sum><rsub|i=1><rsup|10>B<rsub|i>|)><rsup|2>|)>,
  </equation*>

  the square rather seems to be a left superscript of the second closing
  bracket than a right superscript of the first one. This is particularly
  annoying in the case of automatically generated formulas, where this
  situation occurs quite often.

  <section|Implementation>

  The mathematical constructs are typeset by the routines in
  <source-link|Typeset/Concat/concat_math.cpp|src/Typeset/Concat/concat_math.cpp> (fractions, roots, scripts,
  big operators, delimiters, wide accents, <abbr|etc.>), which produce the
  boxes implemented in <source-link|Typeset/Boxes/Composite/math_boxes.cpp|src/Typeset/Boxes/Composite/math_boxes.cpp>
  (fractions, roots, negations, wide accents) and
  <source-link|Typeset/Boxes/Composite/script_boxes.cpp|src/Typeset/Boxes/Composite/script_boxes.cpp> (scripts and limits).
  Brackets are resized in a post-processing step, in
  <source-link|Typeset/Concat/concat_post.cpp|src/Typeset/Concat/concat_post.cpp>, once the entire line is known.
  The spacing between symbols depends on their types (operators,
  relations, <abbr|etc.>), as provided by the mathematical language (see
  <source-link|System/Language/math_language.cpp|src/System/Language/math_language.cpp> and the grammar
  <source-link|progs/language/std-math.scm|TeXmacs/progs/language/std-math.scm>), and on spacing tables provided
  by the font. Notice that the directory
  <source-link|Graphics/Mathematics|src/Graphics/Mathematics> is unrelated to mathematical typesetting:
  it contains generic templates for polynomials, vectors, matrices and
  formal expressions.

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-copyright|2026|the <TeXmacs> team>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
  </collection>
</initial>