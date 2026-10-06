<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Fonts and text in PDF output>

  <section|Drawing a glyph>

  The typesetter draws text one glyph at a time: the boxes of strings call
  <cpp|draw (ch, fn, x, y)> on the renderer with a character code
  <cpp|ch> and a <cpp|font_glyphs> object <cpp|fn>, the low-level glyph
  set of a font at a given size and resolution (see <hlink|fonts|fonts.en.tm>).
  The name of that glyph set, <cpp|fn-\<gtr\>res_name>, identifies the
  font in the <abbr|PDF> file: <verbatim|<em|family>:<em|size>.<em|dpi>>
  for glyphs read with <name|FreeType> (<cpp|tt_font_glyphs>,
  <source-link|tt_face.cpp:241|src/Plugins/Freetype/tt_face.cpp:241>) and
  <verbatim|<em|family><em|size>.<em|dpi>pk> for <name|Metafont> bitmaps
  (<source-link|load_tex.cpp:215|src/Plugins/Metafont/load_tex.cpp:215>).
  The size in points is read back from the name (<cpp|font_size>).

  <cpp|draw> (<source-link|pdf_hummus_renderer.cpp:1468|src/Plugins/Pdf/pdf_hummus_renderer.cpp:1468>)
  proceeds as follows:

  <\enumerate>
    <item>The first time a font is used, <cpp|make_pdf_font> decides
    whether it can be written as a <em|native> font: the part of the name
    before <verbatim|:> is looked up with <cpp|tt_font_find>, and the file
    is loaded with <cpp|PDFWriter::GetFontForFile>, which uses
    <name|FreeType> and accepts Type 1, TrueType, OpenType and <name|CFF>
    fonts. Files listed in <source-link|fonts/pdf-font-issues.scm|TeXmacs/fonts/pdf-font-issues.scm>,
    files which <name|FreeType> cannot open and fonts without a font file
    (the <name|Metafont> fonts rendered to bitmaps) are put in
    <cpp|not_native_fonts> instead.

    <item>When the font changes, a text object is opened (<verbatim|BT>)
    and the font is selected with <verbatim|Tf>: the native font at its
    size, or the Type 3 font of the current <em|chunk> at the conventional
    size 100.

    <item>The glyph is positioned with <verbatim|Td>, relative to the
    previous glyph, and written with <verbatim|Tj>. For a native font, the
    renderer passes to <name|PDFHummus> the index of the glyph in the font
    file (<cpp|gl-\<gtr\>index>) together with the character it stands for,
    as a <cpp|GlyphUnicodeMapping>.
  </enumerate>

  Every glyph is placed individually, so the content stream contains no
  spaces: <abbr|PDF> viewers reconstruct words and spaces from the
  distances between glyphs.

  <section|Native fonts>

  <name|PDFHummus> collects the glyphs used in each native font and, at
  <cpp|EndPDF>, embeds a subset of the font (hence names like
  <verbatim|AAAAAB+CMMI10>) together with a <verbatim|ToUnicode> map built
  from the <cpp|GlyphUnicodeMapping>s. This map is what makes the text
  searchable and copyable. Its quality depends on the character which the
  renderer passes, and the renderer passes the character code <cpp|ch>
  of the glyph set, which is a Unicode code point only for Unicode fonts:

  <\itemize>
    <item>For Unicode fonts (TrueType and OpenType fonts used through
    <cpp|unicode_font>), <cpp|ch> is the code point, and the text layer is
    right.

    <item>For the <name|European Computer Modern> fonts (recognized by
    their PostScript name, <cpp|EuropeanComputerModern_fonts>) the code is a
    Cork code. The ligature glyphs at positions 27 to 31 are mapped to
    U+FB00 to U+FB04, and from <abbr|PDF> 1.5 on each ligature is also
    wrapped in a <verbatim|/Span> with an <verbatim|/ActualText> such as
    <verbatim|(ffi)> (<source-link|pdf_hummus_renderer.cpp:1544|src/Plugins/Pdf/pdf_hummus_renderer.cpp:1544>).
    The other Cork codes are passed as they are, which is wrong for the
    ligature oe, the sharp s, the inverted question and exclamation marks
    and the quotes; <verbatim|wip_fixes> translates them with <cpp|cork_to_utf8>
    (pull request #157, which also handles the <verbatim|T2A> Cyrillic
    fonts).

    <item>For the other <TeX> fonts, in particular the math fonts
    (<verbatim|cmmi>, <verbatim|cmsy>, <verbatim|cmex>, <verbatim|msam>,
    <verbatim|msbm>, ...), the position in the <TeX> encoding is passed as
    a code point, so letters such as <math|\<alpha\>> are lost or replaced
    when the text is copied (issue #302).
  </itemize>

  Two workarounds remain from older <abbr|PDF> viewers: character 0 of the
  Computer Modern fonts is drawn as character 161
  (<cpp|requires_hack_notdef_for_tex_font>), and <verbatim|HelveticaNeue.0.ttf>
  gets fixed font descriptor flags in the library (see <hlink|the
  PDFHummus library|pdf-export-path.en.tm>).

  <section|Type 3 fonts>

  Fonts which cannot be embedded natively are written as Type 3 fonts whose
  glyphs are the bitmaps of the <cpp|font_glyphs>, as inline images with
  one bit per pixel (<cpp|t3font_rep::write_char>,
  <source-link|pdf_hummus_renderer.cpp:1113|src/Plugins/Pdf/pdf_hummus_renderer.cpp:1113>).
  A Type 3 font has at most 256 codes, so a glyph set is split into
  <em|chunks> of 255 characters, each a separate font named
  <verbatim|<em|res_name>-chunk<em|n>> (<cpp|t3font_font_chunk>,
  <cpp|t3font_get_local_glyph>). The glyphs are bitmaps at the printing
  resolution; the font matrix scales them by 1/100, and the font is
  selected at size 100, so that one unit of the glyph is one device pixel.

  <cpp|write_definition> writes, for each chunk, the glyph procedures, the
  widths, the bounding box and a <verbatim|ToUnicode> map which maps each
  local code back to the character code of the glyph set. As for native
  fonts, this character code is only meaningful as Unicode for Unicode
  fonts.

  Type 3 fonts are not scalable: they look grainy when zoomed, and a
  document full of them is large. They usually appear when a font is
  missing and <TeXmacs> falls back to <name|Metafont>, or when a font file
  is listed in <source-link|pdf-font-issues.scm|TeXmacs/fonts/pdf-font-issues.scm>.

  <section|What to check>

  The text layer of an exported file can be checked without a viewer:

  <\verbatim-code>
    mutool draw -F txt -o - <em|file>.pdf \ \ \ \ \ # the text as a viewer
    copies it

    mutool info -F <em|file>.pdf \ \ \ \ \ \ \ \ \ \ \ \ \ \ # the fonts and their types

    mutool draw -F stext -o - <em|file>.pdf \ \ \ # fonts and characters,
    glyph by glyph
  </verbatim-code>

  The document regression tests compare the text layer of sample documents
  with references (<source-link|tests/documents/check.sh|tests/documents/check.sh>).

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
