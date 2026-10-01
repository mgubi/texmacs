<TeXmacs|1.0.3.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Typesetting mathematics>

  <\explain>
    <label|math-level><var-val|math-level|0><explain-synopsis|index level>
  <|explain>
    The <em|index level> increases inside certain mathematical constructs
    such as indices and fractions. When the index level is high, formulas are
    rendered in a smaller font. Nevertheless, index levels higher than
    <verbatim|2> are all rendered in the same way as index level
    <verbatim|2> (more precisely, all index levels beyond the number of
    script sizes specified by <src-var|math-font-sizes> are rendered using
    the smallest size); this ensures that formulas like

    <\equation*>
      \<mathe\><rsup|\<mathe\><rsup|\<mathe\><rsup|\<mathe\><rsup|x>>>>=<frac|1+<frac|1|x+\<mathe\><rsup|x>>|1+<frac|1|\<mathe\><rsup|x>+<frac|1|\<mathe\><rsup|\<mathe\><rsup|x>>>>>
    </equation*>

    remain readable. The index level may be manually changed in
    <menu|Format|Index level>, so as to produce formulas like

    <\equation*>
      x<rsup|<with|math-level|0|y<rsup|<with|math-level|0|z>>>>
    </equation*>

    <\tm-fragment>
      <with|mode|math|<inactive*|x<rsup|<with|math-level|0|y<rsup|<with|math-level|0|z>>>>>>
    </tm-fragment>
  </explain>

  <\explain>
    <var-val|math-font-sizes|default><explain-synopsis|script sizes>
  <|explain>
    This variable determines the font sizes which are used for the different
    index levels. For the value <verbatim|default>, <TeXmacs> uses two
    standard script sizes (for index levels <verbatim|1> and <verbatim|2>).
    Alternatively, the value may be a tuple of tuples of the form
    <explain-macro|tuple|size|s-1|<with|mode|math|\<cdots\>>|s-n>, where
    <src-arg|size> is either a base font size (in points) or
    <verbatim|all>, and where the script sizes <src-arg|s-1> until
    <src-arg|s-n> are either integers (font sizes) or relative sizes like
    <verbatim|*0.8> or <verbatim|*4/5>. For instance, the
    <tmpackage|four-script-sizes> package uses the value

    <\tm-fragment>
      <inactive*|<tuple|<tuple|all|*0.8|*0.6|*0.5>>>
    </tm-fragment>
  </explain>

  <\explain>
    <var-val|math-display|false><explain-synopsis|display style>
  <|explain>
    This environment variable controls whether we are in <em|display style>
    or not. Formulas which occur on separate lines like

    <\equation*>
      <frac|n|H(\<alpha\><rsub|1>,\<ldots\>,\<alpha\><rsub|n>)>=<frac|1|\<alpha\><rsub|1>>+\<cdots\>+<frac|1|\<alpha\><rsub|n>>
    </equation*>

    are usually typeset in display style, contrary to inline formulas like
    <with|mode|math|<frac|n|H(\<alpha\><rsub|1>,\<ldots\>,\<alpha\><rsub|n>)>=<frac|1|\<alpha\><rsub|1>>+\<cdots\>+<frac|1|\<alpha\><rsub|n>>>.
    As you notice, formulas in display style are rendered using a wider
    spacing. The display style is disabled in several mathematical constructs
    such as scripts, fractions, binomial coefficients, and so on. As a
    result, the double numerators in the formula

    <\equation*>
      H(\<alpha\><rsub|1>,\<ldots\>,\<alpha\><rsub|n>)=<frac|n|<frac|1|\<alpha\><rsub|1>>+\<cdots\>+<frac|1|\<alpha\><rsub|n>>>
    </equation*>

    are typeset in a smaller font. You may override the default settings
    using <menu|Format|Display style>.
  </explain>

  <\explain>
    <var-val|math-condensed|false><explain-synopsis|condensed display style>
  <|explain>
    By default, formulas like <with|mode|math|a+\<cdots\>+z> are typeset
    using a nice, wide spacing around the <with|mode|math|<op|+>> symbol. In
    formulas with scripts like <with|mode|math|\<mathe\><rsup|a+\<cdots\>+z>+\<mathe\><rsup|\<alpha\>+\<cdots\>+\<zeta\>>>
    the readability is further enhanced by using a more condensed spacing
    inside the scripts: this helps the reader to distinguish symbols
    occurring in the scripts from symbols occurring at the ground level when
    the scripts are long. The default behaviour can be overridden using
    <menu|Format|Condensed>.
  </explain>

  <\explain>
    <var-val|math-vpos|0><explain-synopsis|position in fractions>
  <|explain>
    For a high quality typesetting of fraction, it is good to avoid
    subscripts in numerators to descend to low and superscripts in
    denominators to ascend to high. <TeXmacs> therefore provides an
    additional environment variable <src-var|math-vpos> which takes the value
    <with|mode|math|1> inside numerators, <with|mode|math|\<um\>1> inside
    denominators and <with|mode|math|0> otherwise. In order to see the effect
    the different settings, consider the following formula:

    <\equation*>
      <with|math-vpos|-1|<group|a<rsub|\<um\>1><rsup|2>>>+<with|math-vpos|0|<group|a<rsub|0><rsup|2>>>+<with|math-vpos|1|<group|a<rsub|1><rsup|2>>>
    </equation*>

    <\tm-fragment>
      <with|mode|math|<inactive*|<with|math-vpos|-1|<group|<active*|a<rsub|\<um\>1><rsup|2>>>>+<with|math-vpos|0|<group|<active*|a<rsub|0><rsup|2>>>>+<with|math-vpos|1|<group|<active*|a<rsub|1><rsup|2>>>>>>
    </tm-fragment>

    In this example, the grouping is necessary in order to let the different
    vertical positions take effect on each
    <with|mode|math|a<rsub|i><rsup|2>>. Indeed, the vertical position is
    uniform for each horizontal concatenation.
  </explain>

  <\explain>
    <var-val|math-nesting-mode|off>

    <src-var|math-nesting-level><explain-synopsis|coloring of nested
    brackets>
  <|explain>
    When <src-var|math-nesting-mode> is not <verbatim|off> (the
    <tmpackage|math-brackets> and <tmpackage|math-check> packages set it to
    <verbatim|colored>), matching large brackets are rendered in colors which
    depend on their nesting depth. The current depth is maintained by the
    typesetter in the variable <src-var|math-nesting-level>.
  </explain>

  <\explain>
    <var-val|math-frac-limit|100par>

    <var-val|math-table-limit|100par>

    <var-val|math-flatten-color|#448><explain-synopsis|flattening of wide
    formulas>
  <|explain>
    Fractions whose numerator or denominator are wider than
    <src-var|math-frac-limit> are typeset in a linear form <math|a/b>
    instead of the usual two-dimensional form; similarly, simple tables
    (without an explicit <src-var|table-width>) which are wider than
    <src-var|math-table-limit> are flattened.
    The brackets and slashes which are introduced for such flattened
    expressions are rendered using the color <src-var|math-flatten-color>.
    The default limits are so large that flattening essentially never
    happens; smaller limits are used for instance in the output of
    interactive sessions.
  </explain>

  <\explain>
    <var-val|math-top-swell-start|1.7ex>

    <var-val|math-top-swell-end|3.5ex>

    <var-val|math-bot-swell-start|-0.7ex>

    <var-val|math-bot-swell-end|-2.5ex><explain-synopsis|extra spacing
    around large formulas>
  <|explain>
    When the paragraph variable <src-var|par-swell> (<abbr|resp.> the cell
    variable <src-var|cell-swell>) is positive, some extra vertical padding
    is added above lines (<abbr|resp.> table cells) whose content rises
    above <src-var|math-top-swell-start>. The padding increases linearly
    with the height and reaches the full value of <src-var|par-swell> at
    <src-var|math-top-swell-end>. The variables
    <src-var|math-bot-swell-start> and <src-var|math-bot-swell-end> play a
    similar role below the baseline.
  </explain>

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>