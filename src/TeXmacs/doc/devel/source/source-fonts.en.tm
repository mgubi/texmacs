<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Fonts>

  These chapters describe how <TeXmacs> finds and uses fonts. The first one
  explains the general philosophy (a font associates graphical meaning to
  words of Cork characters and symbols) and the classical font types; the
  second one describes the font database and the algorithm which selects a
  physical font for the requested family, series and shape; the third one
  describes the layer of smart, emulated and virtual fonts which combines
  several physical fonts and synthesizes missing characters. The fourth one
  describes the support of <name|OpenType>: the layout tables, the
  mathematics driven by the <verbatim|MATH> table, the features of text
  fonts and the profiles of the mathematical fonts. The last one covers
  the fonts of the <TeX> world.

  These chapters are about the implementation. The same subjects as seen by
  users and by authors of documents and style files are described in the
  reference chapter <hlink|fonts, from selection to
  glyph|../fonts/font-guide.en.tm>, in the section <hlink|mathematical
  fonts|../../main/math/fonts/man-math-fonts.en.tm> of the user manual and,
  for the environment variables, in <hlink|specifying the current
  font|../format/environment/env-font.en.tm>.

  <\traverse>
    <branch|<TeXmacs> fonts|fonts.en.tm>

    <branch|The font database and font selection|font-database.en.tm>

    <branch|Smart, virtual and emulated fonts|smart-fonts.en.tm>

    <branch|OpenType fonts|opentype.en.tm>

    <branch|TeX fonts: Metafont, PK, TFM and Type 1|texfonts.en.tm>
  </traverse>

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
