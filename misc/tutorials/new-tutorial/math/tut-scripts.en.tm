<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Subscripts, superscripts and fractions>

  Many mathematical constructs, such as subscripts, superscripts, fractions
  and roots, have one or more parts which you have to fill in. <TeXmacs>
  inserts the construct with empty parts and puts the cursor in the first
  one; you then type its contents and leave it when you are done.

  <paragraph*|Subscripts and superscripts>

  In order to type <math|x<rsup|2>>, type <key|x>, then <key|^> for a
  superscript, and then <key|2>. The cursor is now inside the superscript,
  as indicated by the cyan box:

  <\big-envbox>
    <math|x<rsup|<small-focus|2<math-cursor>>>>
  </big-envbox>

  Whatever you type now goes into the superscript. Press <key*|right> in
  order to move the cursor out of the superscript, and continue the formula.
  Subscripts are obtained in the same way with <key|_>, as in
  <math|x<rsub|n>>.

  <paragraph*|Fractions>

  In order to continue our formula into <math|x<rsup|2>+<frac|a|b>>, type
  <key|+> and then <key|math f> (or select <menu|Insert|Fraction>). An empty
  fraction appears, with the cursor in the numerator:

  <\big-envbox>
    <math|x<rsup|2>+<frac|<small-focus|a<math-cursor>>|>>
  </big-envbox>

  Type <key|a>, then press <key*|down> in order to move to the denominator,
  and type <key|b>:

  <\big-envbox>
    <math|x<rsup|2>+<frac|a|<small-focus|b<math-cursor>>>>
  </big-envbox>

  Finally, press <key*|right> to leave the fraction. In the same way,
  <key|math s> inserts a square root, like in <math|<sqrt|x+1>>, and
  <key|math s var> an <math|n>-th root, like in <math|<sqrt|x+1|3>>.

  <paragraph*|Removing a construct>

  If you inserted a construct by mistake, <key*|backspace> at the start of
  an empty part removes it. More generally, <TeXmacs> keeps track of the
  structure of your formula: the cyan box always shows you in which part of
  which construct the cursor is, and the status bar lists all the
  constructs around the cursor, such as <verbatim|math> and
  <verbatim|denominator>.

  <tmdoc-copyright|1998\U2026|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
