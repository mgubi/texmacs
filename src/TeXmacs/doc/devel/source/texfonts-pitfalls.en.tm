<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|<TeX> fonts: pitfalls and known bugs>

  <section|Things to keep in mind>

  <\itemize>
    <item>The <TeX> helper programs and font directories are detected only
    when the home directory is set up. After installing or removing a
    <TeX> distribution, reset the settings (<verbatim|texmacs -setup>)
    before expecting fonts to be generated, see <hlink|finding and
    generating <TeX> font files|texfonts-files.en.tm>.

    <item>Failures are remembered in <verbatim|$TEXMACS_HOME_PATH/fonts/error>.
    A font which could not be generated once is never tried again until
    this directory is emptied (<verbatim|-delete-font-cache>, an upgrade, or
    a change in the <verbatim|type1> and <verbatim|truetype> font
    directories). A <name|PK> marker is written even when generation is
    disabled, so that enabling a generator later does not help either.

    <item>With a <name|Type 1> substitute, the glyphs come from the
    <verbatim|.pfb> file but the metrics, ligatures and kerning come from the
    <verbatim|.tfm> file. The two must belong to the same font; a
    <verbatim|.pfb> of another design size is scaled, while the metric of
    the requested size is used.

    <item>When <cpp|load_tex> finds neither the requested font nor
    <verbatim|ecrm> at the same size, it ends with <cpp|FAILED>, which aborts
    the current action (see <hlink|fatal errors|server-startup.en.tm>).

    <item><TeX> fonts have at most 256 characters; anything else is
    rendered by the smart and virtual font layers on top of them.
  </itemize>

  <section|Known bugs and suspicious code>

  The following problems were found while writing this chapter; they are
  reported from reading the code, not from runtime tests.

  <\enumerate>
    <item><strong|Positions and drawing disagree on final kerning
    instructions.> In <cpp|tex_font_metric_rep::execute>, which is used for
    drawing, a final instruction (skip byte 128 or more) is still applied if
    it matches the next character, as in <TeX>. In <cpp|get_xpositions>, the
    stop test comes before the match test
    (<source-link|Plugins/Metafont/load_tfm.cpp:264|src/Plugins/Metafont/load_tfm.cpp:264>), so a matching final
    instruction is ignored. The positions computed for selections and the
    cursor can then differ from what is drawn, by the amount of that kern.
    The commented-out code at line 148 shows that <cpp|execute> once had
    the same order and was corrected, but <cpp|get_xpositions> was not.

    <item><strong|Ligatures which keep one of the two characters.> For the
    ligature operations which keep only the left or only the right
    character (operation bytes 1, 2 and their variants with <math|a\<gtr\>0>),
    the stack manipulation in <cpp|execute>
    (<verbatim|load_tfm.cpp:181-183>) and in <cpp|get_xpositions>
    (line 272) seems to put the kept character on the wrong side of the
    ligature: the left character is pushed back when the right one should be
    kept, and conversely. Simple ligatures (<verbatim|fi>, <verbatim|ff>,
    <verbatim|-->, ...) are not affected. Not verified on a font which uses
    these operations.

    <item><strong|Valid <name|PK> files rejected.> The flag byte cases 5 and
    6 (extended short format with a packet length of 65536 or more) are valid
    <name|PK> characters, but <cpp|pk_loader::load_pk> treats them as a loss
    of synchronization and fails (<verbatim|load_pk.cpp:335-343>). Only very
    large characters are concerned.

    <item><strong|Unchecked input in the <verbatim|.tfm> decoder.>
    <cpp|load_tfm> ignores the result of <cpp|load_string>
    (<source-link|load_tfm.cpp:386|src/Plugins/Metafont/load_tfm.cpp:386>) and the <cpp|parse> helpers of
    <source-link|Data/String/analyze.cpp|src/Data/String/analyze.cpp> do not check the string length. The
    consistency test on <cpp|lf> catches most corrupted files, but an empty
    or truncated file is read past its end first. Likewise <cpp|tag (c)> and
    <cpp|rem (c)> (lines 81-82) do not check that <cpp|c> lies in
    <math|[bc,ec]>, unlike the other accessors.

    <item><strong|Wrong destination of <verbatim|maketfm> and
    <verbatim|makepk>.> The <name|Windows> generators are given the
    destination <verbatim|get_env("$TEXMACS_HOME_PATH")> (in
    <source-link|tex_files.cpp:147|src/Plugins/Metafont/tex_files.cpp:147>, <verbatim|180> and <verbatim|185>). The
    environment variable is called <verbatim|TEXMACS_HOME_PATH>, without
    the dollar sign, so the result is empty and the files are written to
    <verbatim|\\fonts\\tfm> and <verbatim|\\fonts\\pk> at the root of the
    current drive, outside the search path.

    <item><strong|Inter-sentence space cannot stretch.> In the
    constructors of <cpp|tex_font_rep> and <cpp|tex_rubber_font_rep>
    (<verbatim|tex_font.cpp:130-131>, <verbatim|tex_rubber_font.cpp:82-83>),
    <cpp|extra-\<gtr\>min> is halved and then <cpp|extra-\<gtr\>max> is set to
    twice the halved minimum, that is (up to rounding) to the default
    value, so the extra space may shrink but never stretch. Doubling the
    default was probably meant.

    <item><strong|Font names do not identify the font.> The resource names
    of <cpp|tex_font> and its variants (<source-link|tex_font.cpp:1110|src/Plugins/Metafont/tex_font.cpp:1110> and
    following) do not contain the fall-back design size <cpp|dsize>, and the
    name of <cpp|tex_rubber_font> (<source-link|tex_rubber_font.cpp:108|src/Plugins/Metafont/tex_rubber_font.cpp:108>) does
    not contain the translator. Two requests which differ only in these
    arguments share the font which was created first.

    <item><strong|Missing glyphs in the metric table.>
    <cpp|tfm_font_metric_rep::get> (<source-link|tex_font.cpp:1086|src/Plugins/Metafont/tex_font.cpp:1086>) uses the
    glyph of a character to compute its ink box without checking that it
    exists. Characters of the <verbatim|.tfm> range which have no glyph in
    the <name|PK> file give a nil glyph. The current callers only ask for
    characters returned by <cpp|index_glyph>, which checks this, so the
    problem is latent.

    <item><strong|Joining of extensible pieces disabled under <name|Qt>.>
    The adjustment of the joining rows of extensible pieces after shrinking
    (<verbatim|Graphics/Bitmap_fonts/glyph_shrink.cpp:235-240>) is compiled
    only without <verbatim|QTTEXMACS>, although <cpp|rubber_fix> still marks
    the pieces in all builds.

    <item><strong|Dead and stale code.> In <cpp|try_pk>
    (<source-link|load_tex.cpp:207|src/Plugins/Metafont/load_tex.cpp:207>), the test
    <cpp|font_glyphs::instances-\<gtr\>contains (tt_name)> can never succeed,
    because <cpp|tt_font_glyphs> registers its tables under another name;
    the function works because <cpp|tt_font_glyphs> itself returns the
    cached instance. The comment <verbatim|we need pfbtopfa> which excludes
    <name|Type 1> embedding in PostScript on <name|Windows>
    (<source-link|Graphics/Renderer/printer.cpp:490|src/Graphics/Renderer/printer.cpp:490>) is stale: the conversion
    is done by the internal <cpp|pfb_to_pfa>.
  </enumerate>

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
