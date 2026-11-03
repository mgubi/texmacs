<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The <TeX> font classes and their output>

  This page describes the font classes built on <TeX> fonts
  (<source-link|Plugins/Metafont/tex_font.cpp|src/Plugins/Metafont/tex_font.cpp> and
  <source-link|tex_rubber_font.cpp|src/Plugins/Metafont/tex_rubber_font.cpp>) and how their glyphs end up on the screen,
  in <abbr|PDF> and in PostScript. The general interface of fonts
  (<cpp|font_rep>: extents, positions, corrections, <cpp|index_glyph>) is
  described in <hlink|<TeXmacs> fonts|fonts.en.tm>.

  <section|Text fonts>

  <\explain>
    <cpp|struct tex_font_rep: font_rep><explain-synopsis|a <TeX> text font>
  <|explain>
    Its own fields are the <cpp|status> (the variant, see below), the
    <cpp|family>, the resolution <cpp|dpi>, the fall-back design size
    <cpp|dsize>, the metric <cpp|tfm>, the glyphs <cpp|pk> (from a
    <name|Type 1> substitute or a <name|PK> file), the conversion factor
    <cpp|unit> from <verbatim|.tfm> units to <TeXmacs> units, and the flag
    <cpp|exec>, which says whether the ligature and kerning program is run;
    it is false for typewriter families (whose name ends in
    <verbatim|tt>).

    The constructor calls <cpp|load_tex> and fills the generic font
    parameters of <cpp|font_rep> from the metric: <cpp|design_size> and
    <cpp|display_size> (in points and in units at the given resolution),
    <cpp|slope>, the interword space <cpp|spc> with its stretch and shrink,
    <cpp|extra>, the x-height <cpp|yx> and the derived script positions
    (<cpp|ysub_lo_base>, <cpp|ysup_lo_base>, ...), the fraction bar height
    <cpp|yfrac>, the rule width <cpp|wline> and the quad <cpp|wquad>. For
    <verbatim|cmr>, <verbatim|ecrm> and <verbatim|cmmi>, the rule width and
    the fraction bar position are tuned per size. It also installs the
    script and accent correction tables of <source-link|adjust_cmr.cpp|src/Plugins/Metafont/adjust_cmr.cpp> for
    the families which have them (<verbatim|ecrm>/<verbatim|ecbx>,
    <verbatim|ecss>/<verbatim|ecsx>, <verbatim|cmr>/<verbatim|cmbx>,
    <verbatim|cmmi>/<verbatim|cmmib>, <verbatim|cmsy>/<verbatim|cmbsy>,
    <verbatim|bbm>, <verbatim|eufm>/<verbatim|eufb>, <verbatim|rsfs>);
    these tables, keyed by character, shift sub- and superscripts and wide
    accents by a fraction of the font size, see <hlink|mathematical
    typesetting|maths.en.tm>.
  </explain>

  <paragraph|Variants.>The same class implements six kinds of fonts, which
  differ in how strings of <TeXmacs> characters are mapped to the 256
  character codes of the font. Each has a constructor and a font tree tag
  recognized by <cpp|find_font>:

  <\description-paragraphs>
    <item*|<cpp|tex_font>, tree <verbatim|(tex <em|family> <em|size>
    <em|dpi> [<em|dsize>])>>Status <cpp|TEX_ANY>: raw character codes; only
    one-character strings and <verbatim|\<less\>less\<gtr\>>,
    <verbatim|\<less\>gtr\<gtr\>> are supported. Used for symbol fonts such
    as <verbatim|cmsy>, <verbatim|msam> or <verbatim|stmary>.

    <item*|<cpp|tex_ec_font>, tree <verbatim|(ec ...)>>Status
    <cpp|TEX_EC>: fonts in the Cork (<verbatim|T1>) encoding, which is also
    the internal encoding of <TeXmacs> strings, so that strings are passed
    through unchanged.

    <item*|<cpp|tex_la_font>, <cpp|tex_gr_font>, trees <verbatim|(la ...)>,
    <verbatim|(gr ...)>>Status <cpp|TEX_LA> and <cpp|TEX_GR>: Cyrillic
    (<verbatim|larm>, ...) and Greek fonts, with sizes in the
    <math|\<times\>100> convention. Characters written as entities, such as
    <verbatim|\<less\>#41F\<gtr\>>, are translated to font codes through the
    encodings <verbatim|larm> and <verbatim|grmn> (a table
    <cpp|special_translate> built once from these translators, in lower and
    upper case).

    <item*|<cpp|tex_cm_font>, <cpp|tex_adobe_font>, trees <verbatim|(cm
    ...)>, <verbatim|(adobe ...)>>Status <cpp|TEX_CM> and
    <cpp|TEX_ADOBE>: fonts in the old <TeX> text encoding (<verbatim|OT1>) or
    in the encoding of the <name|Adobe> clones, which lack the accented
    letters of the Cork encoding. Such letters are typeset as the base
    letter with an accent drawn above it, using two tables
    (<cpp|CM_unaccented>/<cpp|CM_accents> or the <name|Adobe> ones) which
    give, for each Cork code from 128 on, the base letter and the accent
    character. The position of the accent depends on the height of the
    letter and on the slant of the font; a few accents which go below or
    beside the letter, such as the cedilla, are placed by special rules.
  </description-paragraphs>

  For all variants, a string is drawn by running the ligature and kerning
  program of the metric (<cpp|tfm-\<gtr\>execute>) and drawing the resulting
  character codes one after the other, each followed by its width and
  kerning. <cpp|get_extents> and <cpp|get_xpositions> compute the same
  layout without drawing. <cpp|supports (s)> says which strings the font
  can render; for <cpp|TEX_ANY>, a character is supported if the font has a
  non-empty glyph for it. <cpp|magnify (zx, zy)> creates the same kind of
  font at a resolution multiplied by the zoom factor, or a
  <cpp|poor_magnify> wrapper if the two factors differ.

  <paragraph|Glyph access.><cpp|get_glyph (s)> and <cpp|index_glyph (s,
  fnm, fng)> map a string to a single glyph: a one-character string is its
  code, a longer string is accepted if the ligature program reduces it to a
  single character (<cpp|get_ligature_code>, for instance
  <verbatim|ffi>). <cpp|index_glyph> returns the code together with the
  metric table <cpp|tfm_font_metric (tfm, pk, unit)> and the glyph table
  <cpp|pk>, which is what the renderers and the virtual and smart fonts use
  (see <hlink|smart fonts|smart-fonts.en.tm>). Strings which are not a
  single glyph fall back to the generic implementations of
  <cpp|font_rep>.

  <section|Rubber fonts>

  <\explain>
    <cpp|struct tex_rubber_font_rep: font_rep><explain-synopsis|extensible
    characters>
  <|explain>
    Created by <cpp|tex_rubber_font (trl, family, size, dpi, dsize)> from a
    tree <verbatim|(tex-rubber <em|translator> <em|family> <em|size>
    <em|dpi> [<em|dsize>])>, for instance <verbatim|(tex-rubber rubber-cmex
    cmex 10 600)> in <source-link|progs/fonts/fonts-adobe.scm|TeXmacs/progs/fonts/fonts-adobe.scm>. It supports the
    strings <verbatim|\<less\>left-<em|x>-<em|n>\<gtr\>>,
    <verbatim|\<less\>right-...\<gtr\>>, <verbatim|\<less\>mid-...\<gtr\>>,
    <verbatim|\<less\>large-...\<gtr\>> and
    <verbatim|\<less\>big-...\<gtr\>>, where <em|n> is a size number.
  </explain>

  To render <verbatim|\<less\>left-(-3\<gtr\>>, the font looks up the base
  character <verbatim|\<less\>left-(\<gtr\>> in the translator (here
  <verbatim|rubber-cmex>), which gives a character code of the font, and
  follows the chain of successively larger characters of the metric
  (<cpp|nth_in_list>) <em|n> times. If the chain ends in an extensible
  character (tag 3) before reaching size <em|n>, the character is assembled
  from its top, middle, bottom and repeated pieces, with as many
  repetitions as needed for the remaining size steps. While drawing, the
  vertical position is rounded to whole pixels after each piece, and on
  printers each repeated piece is drawn a second time slightly lower, so
  that no gaps appear between the pieces. <cpp|get_extents> adds some
  horizontal space around large delimiters, and corrects the vertical
  extents of the largest big operators
  (<verbatim|\<less\>big-...-2\<gtr\>>).

  <cpp|tex_dummy_rubber_font (fn)>, for the tree <verbatim|(tex-dummy-rubber
  ...)>, renders the invisible delimiters <verbatim|\<less\>left-.\<gtr\>>,
  <verbatim|\<less\>big-.\<gtr\>>, and so on: it takes the height of the
  corresponding parenthesis (or of <verbatim|\<less\>big-sum\<gtr\>>) in
  <cpp|fn>, with zero width, and draws nothing.

  <section|Output>

  <paragraph|On the screen.>The renderer receives each character as
  <cpp|draw (c, fng, x, y)>, with the glyph table of the font. It shrinks
  the glyph to the screen resolution and caches the result, see <hlink|the
  glyph section|texfonts-formats.en.tm> and <hlink|renderer
  back-ends|renderer-backends.en.tm>.

  <paragraph|In <abbr|PDF>.><cpp|pdf_hummus_renderer_rep::draw (ch, fn, x,
  y)> (<source-link|Plugins/Pdf/pdf_hummus_renderer.cpp|src/Plugins/Pdf/pdf_hummus_renderer.cpp>) decides once per glyph
  table how to embed it (<cpp|make_pdf_font>). The part of the glyph table
  name before the first colon is taken as a font name and looked up with
  <cpp|tt_font_find>; for a <name|Type 1> substitute the glyph table is
  called <verbatim|<em|family><em|size>:<em|size>.<em|dpi>tt>, so this finds
  the <verbatim|.pfb> file, which is then embedded as a native font, unless
  its file name is listed in <verbatim|$TEXMACS_PATH/fonts/pdf-font-issues.scm>.
  <name|PK> glyph tables (named
  <verbatim|<em|family><em|size>.<em|dpi>pk>) and fonts which cannot be
  loaded are embedded as <name|Type 3> fonts made of bitmaps, in chunks of
  about 255 characters (a <TeX> font always fits in one chunk), rendered at a conventional size of 100. Two
  workarounds concern <TeX> fonts: character 0 of the Computer Modern
  families is drawn as character 161 (for old <abbr|PDF> viewers), and for
  native European Computer Modern fonts the ligatures <verbatim|ff> to
  <verbatim|ffl> (codes 27 to 31) are mapped to the Unicode ligature code
  points from <verbatim|U+FB00> on, so that text can be copied from the
  <abbr|PDF> file.

  <paragraph|In PostScript.><cpp|printer_rep::draw>
  (<source-link|Graphics/Renderer/printer.cpp|src/Graphics/Renderer/printer.cpp>) records each character used.
  When the document is finished, <cpp|generate_tex_fonts> writes, for each
  glyph table, either the <name|Type 1> font converted from
  <verbatim|.pfb> to <verbatim|.pfa> (<cpp|pfb_to_pfa>) together with a
  re-encoding and scaling, or, for <name|PK> glyphs, a bitmap font with one
  hexadecimal bitmap per character. The <name|Type 1> path is not compiled
  on <name|Windows>.

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
