<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Smart fonts, emulated fonts and virtual fonts>

  <section|Introduction>

  The <hlink|overview of fonts|fonts.en.tm> explains the general
  philosophy of fonts in <TeXmacs>: a font is an object which associates a
  graphical meaning to <em|words>, that is to strings of Cork characters and
  universal symbols like <verbatim|\<less\>alpha\<gtr\>> or
  <verbatim|\<less\>#2212\<gtr\>>. The present chapter describes the layer
  which makes this philosophy work in practice with real, incomplete
  physical fonts. It consists of three cooperating mechanisms:

  <\description>
    <item*|Smart fonts>A <em|smart font> (<cpp|smart_font_rep> in
    <verbatim|Graphics/Fonts/smart_font.cpp>) is the font which the
    typesetter actually uses when the preference <verbatim|"new style
    fonts"> is on. It is a dispatcher: it cuts each string into runs of
    characters, finds for every character a <em|subfont> which can render
    it, possibly <em|rewrites> the characters into the encoding expected by
    that subfont, and delegates measuring and drawing. Subfonts are created
    lazily and the decisions are cached.

    <item*|Virtual fonts>A <em|virtual font> (<cpp|virtual_font_rep> in
    <verbatim|Graphics/Fonts/virtual_font.cpp>) builds new glyphs out of
    glyphs of a base font, following definitions written in a small
    <scheme>-like language in the files
    <verbatim|$TEXMACS_PATH/fonts/virtual/*.vfn>. Long arrows, negated
    relations, missing brackets, integrals with several signs or extensible
    delimiters are all obtained in this way when the physical font does not
    provide them.

    <item*|Emulated fonts>The \Ppoor man's\Q fonts in
    <verbatim|Graphics/Fonts/poor_*.cpp> synthesize variants which do not
    exist physically: bold, italic, small capitals, blackboard bold,
    stretched, extended or condensed, monospaced, distorted or blurred
    variants of an existing font, as well as extensible delimiters. They
    are used automatically when the font database does not contain the
    requested series or shape, and explicitly through the
    <src-var|font-effects> environment variable.
  </description>

  The three mechanisms are composable: a smart font may have virtual fonts
  and emulated fonts among its subfonts, a virtual font may be built on top
  of a smart font or of an emulated font, and the font effects are applied
  on top of the smart font. All of them are subclasses of the abstract
  <cpp|font_rep> (see <verbatim|Graphics/Fonts/font.hpp> and the
  <hlink|overview|fonts.en.tm>) and are therefore interchangeable from the
  point of view of the typesetter and of the <hlink|renderers|renderer.en.tm>.

  The selection of a <em|physical> font from a family name, variant, series
  and shape (the font database, <cpp|logical_font>, <cpp|search_font>,
  <cpp|find_closest> and <cpp|closest_font>) is documented separately in the
  chapter on the <hlink|font database and font selection|font-database.en.tm>.
  Here we take <cpp|closest_font> as a black box which returns the best
  available physical font, and we concentrate on what happens around it.

  <section|Why smart fonts?>

  Physical fonts are always incomplete. A text font usually covers Latin and
  perhaps Greek and Cyrillic, but not mathematical symbols; a mathematical
  <name|OpenType> font has most symbols but no Chinese characters; few fonts
  contain bold calligraphic letters, double struck digits or a long
  <verbatim|\<less\>leftrightarrow\<gtr\>>. On the other hand, a <TeXmacs>
  document is written using a font independent encoding, and the user
  expects that every symbol is rendered, in a style as close as possible to
  the chosen font. Smart fonts reconcile these requirements:

  <\itemize>
    <item>Characters missing in the main font are taken from other fonts:
    explicitly mentioned fonts (the <src-var|font> variable may contain a
    comma separated list of families, possibly with conditions such as
    <verbatim|mathlarge=TeX Gyre Pagella,Linux Libertine>), and then from
    progressively less similar fonts found in the font database for the
    relevant <name|Unicode> range.

    <item>Fallback fonts are scaled so that their x-height matches the one of
    the main font (<cpp|smart_font_rep::adjusted_dpi>).

    <item>Mathematical conventions are implemented at the font level: in
    mathematical mode isolated Latin letters are taken from the italic
    variant of the font, Greek letters are taken from the italic math
    alphabet if available, <name|Unicode> mathematical alphanumeric symbols
    (<verbatim|U+1D400> and following) are mapped to bold, calligraphic,
    fraktur or double struck variants of the current family, and so on.

    <item>Symbols which no font provides are constructed with virtual fonts
    or emulated fonts, and only as a last resort drawn in red using the
    <em|error font>.
  </itemize>

  <section|Overview of the layers>

  The following picture summarizes the objects involved when the
  typesetter asks the current font to measure or draw a string.

  <\verbatim-code>
    edit_env_rep::update_font\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (Typeset/Env/env_semantics.cpp)

    \ \ fn = smart_font (family, variant, series, shape, size, dpi)

    \ \ fn = apply_effects (fn, font-effects)\ \ \ \ -\<gtr\> poor_bold, poor_italic, ...

    \;

    smart_font_rep\ \ (one per family/variant/series/shape/size/dpi)

    \ \ sm : smart_map (shared by all sizes)\ \ \ \ \ char -\<gtr\> subfont number

    \ \ fn : array\<less\>font\<gtr\>\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ subfonts, created lazily

    \ \ \ \ fn[0]\ \ main font\ \ = closest_font (main family, ...)

    \ \ \ \ fn[1]\ \ error font = error_font (closest_font ("roman", "ss", ...))

    \ \ \ \ fn[k]\ \ e.g.\ \ closest_font (other family or range, ...)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ smart_font_bis (family, "outline", ...)\ \ \ for bbb

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ virtual_font (this, "tradi-long", ...)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ virtual_font (main, "emu-arrows", ..., true)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ poor_bold_font (...), poor_bbb_font (...)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ rubber_font (fn[j])\ \ \ -\<gtr\> poor_rubber_font, ...
  </verbatim-code>

  The rest of this chapter is organized as follows:

  <\traverse>
    <branch|The smart font: dispatching characters to
    subfonts|smart-fonts-smart.en.tm>

    <branch|The character resolution algorithm|smart-fonts-resolve.en.tm>

    <branch|Virtual fonts and the vfn language|smart-fonts-virtual.en.tm>

    <branch|Emulated (poor man's) fonts|smart-fonts-emulated.en.tm>

    <branch|Font effects|smart-fonts-effects.en.tm>

    <branch|Older composite fonts: compound and math
    fonts|smart-fonts-compound.en.tm>

    <branch|Extending, debugging and pitfalls|smart-fonts-howto.en.tm>
  </traverse>

  <section|Source files>

  <\description-paragraphs>
    <item*|<verbatim|Graphics/Fonts/smart_font.cpp>>The smart font, the
    <cpp|smart_map> cache, the rewriting rules, the user entry points
    <cpp|smart_font>, <cpp|smart_font_bis> and <cpp|apply_effects>.

    <item*|<verbatim|Graphics/Fonts/virtual_font.cpp>>Virtual fonts:
    compilation of <verbatim|.vfn> definitions into glyphs, metrics and
    vector drawing. <verbatim|Graphics/Fonts/virtual_enhance.cpp> adds
    virtual definitions to an existing font.

    <item*|<verbatim|Graphics/Fonts/translator.cpp>>Loading of
    <verbatim|.enc> encodings and <verbatim|.vfn> files into
    <cpp|translator> objects.

    <item*|<verbatim|Graphics/Fonts/poor_*.cpp>>Emulated fonts:
    <verbatim|poor_bold>, <verbatim|poor_italic>,
    <verbatim|poor_smallcaps>, <verbatim|poor_bbb>,
    <verbatim|poor_stretched>, <verbatim|poor_extended>,
    <verbatim|poor_mono>, <verbatim|poor_distorted>,
    <verbatim|poor_effected> and <verbatim|poor_rubber>, together with
    <verbatim|recolored_font.cpp> and <verbatim|superposed_font.cpp>.

    <item*|<verbatim|Graphics/Fonts/font.cpp>>The base class, the error font
    and <cpp|rubber_font>.

    <item*|<verbatim|Graphics/Fonts/compound_font.cpp>,
    <verbatim|math_font.cpp>, <verbatim|charmap.cpp>>Older composite fonts
    used by the rule based font selection.

    <item*|<verbatim|Graphics/Bitmap_fonts/>>Glyph (bitmap) manipulation
    routines used by virtual and emulated fonts (<cpp|join>,
    <cpp|hor_flip>, <cpp|bolden>, <cpp|slanted>, <cpp|make_bbb>, ...).

    <item*|<verbatim|$TEXMACS_PATH/fonts/virtual/>>The virtual font
    definitions <verbatim|tradi-*.vfn> and <verbatim|emu-*.vfn>.
  </description-paragraphs>

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
