<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|<TeX> fonts: Metafont, PK, TFM and Type 1>

  <section|Introduction>

  The fonts which <TeXmacs> uses by default for text and mathematics (the
  European Computer Modern fonts <verbatim|ec*>, the Computer Modern fonts
  <verbatim|cm*>, the <abbr|AMS> symbol fonts, the <verbatim|cmex> rubber
  characters, and so on) are <TeX> fonts. A <TeX> font consists of a
  <em|font metric file> (<verbatim|.tfm>) which gives the dimensions,
  ligatures, kerning and extensible recipes of at most 256 characters, and
  of the <em|glyphs> themselves, which come either from a <name|Type 1> outline
  font (<verbatim|.pfb>) or from a bitmap font in <name|PK> format
  (<verbatim|.<em|dpi>pk>), possibly generated on the fly by <name|Metafont>.

  This chapter describes how <TeXmacs> finds, generates, decodes, typesets and
  outputs such fonts. The general font model (the class <cpp|font_rep>, font
  names and font trees) is explained in <hlink|<TeXmacs>
  fonts|fonts.en.tm>; how a logical font request is mapped to a font tree in
  <hlink|the font database and font selection|font-database.en.tm>; how
  missing characters are emulated and how fonts are combined in
  <hlink|smart, virtual and emulated fonts|smart-fonts.en.tm>; and how the
  renderers draw glyphs in <hlink|the renderer interface|renderer.en.tm>.
  These subjects are only summarized here.

  All file names are relative to <verbatim|src/src/> unless stated
  otherwise.

  <section|Overview>

  A <TeX> font is requested by a <em|font tree> such as
  <verbatim|(ec ecrm 10 600)>, which the font rules of
  <source-link|TeXmacs/progs/fonts/fonts-ec.scm|TeXmacs/progs/fonts/fonts-ec.scm> (relative to <verbatim|src/>)
  produce for the roman family, and which
  <cpp|find_font> (<source-link|Graphics/Fonts/find_font.cpp|src/Graphics/Fonts/find_font.cpp>) turns into a call
  of one of the constructors <cpp|tex_font>, <cpp|tex_ec_font>,
  <cpp|tex_cm_font>, <cpp|tex_la_font>, <cpp|tex_gr_font>,
  <cpp|tex_adobe_font>, <cpp|tex_rubber_font> or
  <cpp|tex_dummy_rubber_font>. From there, the data flow is:

  <\verbatim-code>
    font tree (ec ecrm 10 600)

    \ \ \|

    \ \ v

    tex_font_rep \ \ (Plugins/Metafont/tex_font.cpp)

    \ \ \|-- load_tex (family, size, dpi, dsize) \ \ (load_tex.cpp)

    \ \ \| \ \ \ \ \|-- metrics: load_tex_tfm -\<gtr\> resolve_tex ("ecrm10.tfm")

    \ \ \| \ \ \ \ \| \ \ \ \ \ \ \ \ \ \ \ \ -\<gtr\> load_tfm \ -\<gtr\> tex_font_metric

    \ \ \| \ \ \ \ \|-- glyphs: \ load_tex_pk -\<gtr\> Type 1 file found?

    \ \ \| \ \ \ \ \ \ \ \ \ \ yes: tt_font_glyphs (FreeType rasterizes the .pfb)

    \ \ \| \ \ \ \ \ \ \ \ \ \ no: \ resolve_tex ("ecrm10.600pk"), mktexpk if

    \ \ \| \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ needed -\<gtr\> pk_loader (lazy unpacking)

    \ \ \|

    \ \ \|-- screen: \ renderer shrinks the glyph bitmap, caches the pixmap

    \ \ \|-- PDF: \ \ \ \ Type 1 embedded natively, PK glyphs as Type 3 fonts

    \ \ \|-- PS: \ \ \ \ \ Type 1 embedded as PFA, PK glyphs as bitmap fonts
  </verbatim-code>

  Locating files goes through a search path built at startup from the
  settings of <verbatim|$TEXMACS_HOME_PATH/system/settings.scm>, through the
  <TeX> utility <verbatim|kpsewhich> if it is available, and through the
  persistent cache <verbatim|font_cache.scm>. Files which could not be found
  or generated are remembered as empty marker files in
  <verbatim|$TEXMACS_HOME_PATH/fonts/error>, so that the (slow) generation is
  not attempted again.

  <TeXmacs> ships its own copies of the most common <TeX> fonts in
  <verbatim|$TEXMACS_PATH/fonts/tfm> and <verbatim|$TEXMACS_PATH/fonts/type1>
  (subdirectories <verbatim|ec>, <verbatim|la>, <verbatim|math>,
  <verbatim|ams>, <verbatim|adobe>, <verbatim|tc>, <verbatim|cbgreek>,
  <verbatim|public>, ...), so that in the usual case no <TeX> installation
  is needed and no font is generated. The metrics are shipped at many sizes
  (for instance about 250 <verbatim|.tfm> files for the <verbatim|ec>
  families) and the outlines at the design sizes only (45 <verbatim|.pfb>
  files for <verbatim|ec>); <cpp|tt_find_name> picks the outline of a
  nearby design size, so that for these families the <name|PK> path is
  never taken.

  <section|Source files>

  <\description-paragraphs>
    <item*|<source-link|Plugins/Metafont/tex_files.hpp|src/Plugins/Metafont/tex_files.hpp>,
    <source-link|tex_files.cpp|src/Plugins/Metafont/tex_files.cpp>>Search paths for <verbatim|.tfm>,
    <verbatim|.pk> and <verbatim|.pfb> files (<cpp|reset_tfm_path>,
    <cpp|reset_pk_path>, <cpp|reset_pfb_path>), lookup
    (<cpp|resolve_tex>, <cpp|exists_in_tex>, <verbatim|kpsewhich>,
    <verbatim|kpsepath>) and generation (<cpp|make_tex_tfm>,
    <cpp|make_tex_pk>).

    <item*|<source-link|Plugins/Metafont/tex_init.cpp|src/Plugins/Metafont/tex_init.cpp>>Detection of the <TeX>
    helper programs and of the standard <TeX> font directories at the first
    run (<cpp|setup_tex>), and initialization of the paths at each run
    (<cpp|init_tex>).

    <item*|<source-link|Plugins/Metafont/load_tex.hpp|src/Plugins/Metafont/load_tex.hpp>,
    <source-link|load_tex.cpp|src/Plugins/Metafont/load_tex.cpp>>The search for a metric and glyphs of a given
    family, size and resolution (<cpp|load_tex>, <cpp|load_tex_tfm>,
    <cpp|load_tex_pk>), including the substitution by <name|Type 1> fonts and
    the error cache.

    <item*|<source-link|Plugins/Metafont/load_tfm.hpp|src/Plugins/Metafont/load_tfm.hpp>,
    <source-link|load_tfm.cpp|src/Plugins/Metafont/load_tfm.cpp>>The class <cpp|tex_font_metric_rep>: decoding of
    <verbatim|.tfm> files, the ligature and kerning program, extensible
    recipes.

    <item*|<source-link|Plugins/Metafont/load_pk.hpp|src/Plugins/Metafont/load_pk.hpp>,
    <source-link|load_pk.cpp|src/Plugins/Metafont/load_pk.cpp>>The <cpp|pk_loader>, which decodes
    <verbatim|.pk> files into glyphs.

    <item*|<source-link|Plugins/Metafont/tex_font.cpp|src/Plugins/Metafont/tex_font.cpp>>The class
    <cpp|tex_font_rep> and its six variants, and <cpp|tfm_font_metric>.

    <item*|<source-link|Plugins/Metafont/tex_rubber_font.cpp|src/Plugins/Metafont/tex_rubber_font.cpp>>Rubber
    (extensible) characters from <TeX> fonts: <cpp|tex_rubber_font_rep> and
    <cpp|tex_dummy_rubber_font_rep>.

    <item*|<source-link|Plugins/Metafont/adjust_cmr.cpp|src/Plugins/Metafont/adjust_cmr.cpp>>Hand-tuned script
    and accent position corrections for the Computer Modern and related
    families.

    <item*|<source-link|Graphics/Bitmap_fonts/bitmap_font.hpp|src/Graphics/Bitmap_fonts/bitmap_font.hpp>,
    <source-link|glyph.cpp|src/Graphics/Bitmap_fonts/glyph.cpp>, <source-link|bitmap_font.cpp|src/Graphics/Bitmap_fonts/bitmap_font.cpp>,
    <source-link|glyph_shrink.cpp|src/Graphics/Bitmap_fonts/glyph_shrink.cpp>>The classes <cpp|glyph>,
    <cpp|font_metric> and <cpp|font_glyphs>, and the shrinking of glyphs for
    display. The other files of this directory implement glyph
    transformations, which are described in <hlink|emulated
    fonts|smart-fonts-emulated.en.tm>.

    <item*|<source-link|Plugins/Freetype/tt_file.cpp|src/Plugins/Freetype/tt_file.cpp>,
    <source-link|tt_face.cpp|src/Plugins/Freetype/tt_face.cpp>>Location of <verbatim|.pfb> files and their
    rasterization through <name|FreeType> (<cpp|tt_find_name>,
    <cpp|tt_font_find>, <cpp|tt_font_glyphs>).

    <item*|<source-link|Plugins/Pdf/pdf_hummus_renderer.cpp|src/Plugins/Pdf/pdf_hummus_renderer.cpp>,
    <source-link|Graphics/Renderer/printer.cpp|src/Graphics/Renderer/printer.cpp>>Embedding of <TeX> fonts in
    <abbr|PDF> and PostScript output.

    <item*|<source-link|TeXmacs/progs/fonts/fonts-ec.scm|TeXmacs/progs/fonts/fonts-ec.scm>,
    <source-link|fonts-composite.scm|TeXmacs/progs/fonts/fonts-composite.scm>, <source-link|fonts-adobe.scm|TeXmacs/progs/fonts/fonts-adobe.scm>,
    <source-link|fonts-math.scm|TeXmacs/progs/fonts/fonts-math.scm>>(relative to <verbatim|src/>) Font rules which
    map logical font requests to <TeX> font trees.
  </description-paragraphs>

  <section|Contents of this chapter>

  <\traverse>
    <branch|Finding and generating <TeX> font files|texfonts-files.en.tm>

    <branch|TFM metrics, PK glyphs and Type 1 substitution|texfonts-formats.en.tm>

    <branch|The <TeX> font classes and their output|texfonts-classes.en.tm>

    <branch|Pitfalls and known bugs|texfonts-pitfalls.en.tm>
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
