<TeXmacs|2.1.5>

<style|tmdoc>

<\body>
  <tmdoc-title|From markup to glyph>

  Between the font you asked for and the ink on the page there are several
  steps, and each of them can be configured. This page follows one character
  through them.

  <paragraph*|What the typesetter sees>

  The text of a document is a sequence of <em|characters> in the internal
  encoding of <TeXmacs>: ASCII characters and the accented Latin letters of
  the Cork encoding stand for themselves, and everything else is written
  between angle brackets. The letter
  <math|<with|font-shape|italic|\<alpha\>>> is the character
  <verbatim|\<less\>alpha\<gtr\>>, and a character which has neither a
  byte nor a name is written by its Unicode code point, such as
  <verbatim|\<less\>#1D6FC\<gtr\>>. Stretchable characters have names of
  their own, such as <verbatim|\<less\>left-(-2\<gtr\>> for the third
  size of an opening parenthesis (sizes count from 0).

  A name is not a glyph, and it is not a code point either: it is what the
  editor stores and what the font system is asked to draw.

  <paragraph*|The logical font>

  When the typesetter reaches a piece of text it reads the mode it is in,
  text, mathematics or program, and the corresponding variables
  (<hlink|selecting fonts|font-selection.en.tm>). From them it builds a
  <em|logical font>: a family name followed by normalized features, such as
  <verbatim|(Pagella bold italic)>. The font a document asks for is a
  request in the same sense: the family may not exist on this machine, and
  the features may not all be available.

  <paragraph*|The smart font>

  What the typesetter receives back is a <em|smart font>. It is not one
  font: it is a router which keeps a list of subfonts and decides, character
  by character and once for each, which of them draws it. This is why a
  formula can mix a Greek letter from the math font, a blackboard bold
  letter which is drawn by hand and an arrow taken from a third font,
  without the document saying anything about it.

  The decision follows a ladder, from the most faithful to the most
  desperate:

  <\enumerate>
    <item>a few rewritings which come first: in a formula an italic Greek
    letter, a dotless letter or a prime taken from the italic font, a few
    special characters, and the brackets which <TeXmacs> knows how to build
    out of pieces;

    <item>the families of the font request, the main one first, in
    successive attempts, each one less demanding than the last: the font
    itself, its derived, or \Ppoor\Q, variants (bold by thickening, for
    instance), its stretchable characters, and at the later attempts
    another family which has the character, found in the font database and
    rendered at a size adjusted so that its x-height matches the one of the
    main font. A symbol the main font lacks may also be emulated there, as
    a construction over its other glyphs; when the main font is an
    <name|OpenType> math font and the construction could only be exported
    as a bitmap, the symbol is taken from <name|STIX Two Math> instead,
    which comes with <TeXmacs>, so that the document looks the same on
    every system;

    <item>the mathematical letters: a bold, script, fraktur or
    double-struck letter taken from the Unicode mathematical alphanumerics
    or from an emulated alphabet;

    <item>a <hlink|virtual font|virtual-fonts.en.tm> which defines the
    character as a construction over other glyphs;

    <item>failing everything, the error font, which draws the name of the
    character in red.
  </enumerate>

  A red name in a formula therefore does not mean that the character is
  unknown to <TeXmacs>: it means that no font was found for it. The most
  common cause is a font database which was written before the font you
  need was installed; see <hlink|the font configuration
  files|font-config.en.tm>.

  <paragraph*|From a name to a code point>

  Steps 2 and 6 need to know which Unicode character a name stands for. The
  tables of <verbatim|$TEXMACS_PATH/langs/encoding> answer that question:
  <verbatim|tmuniversaltounicode.scm> and its companions map
  <verbatim|\<less\>alpha\<gtr\>> to <verbatim|U+03B1> and back. The same
  tables serve the converters, which is why a symbol without an entry there
  can be typed and printed but not exported.

  <paragraph*|From a family to a file>

  Choosing the file is the work of the font database. A logical font is
  compared with every style of every family it knows, by a distance which
  counts the features that match and the ones that had to be dropped; the
  first attempt is strict, the following ones are increasingly tolerant,
  which is how the fallback of step<nbsp>6 finds a font for a rare
  character. The files which hold that knowledge are described in
  <hlink|the font configuration files|font-config.en.tm>.

  The <TeX> fonts are addressed differently: their metrics come from a
  <verbatim|.tfm> file and their glyph positions have no Unicode meaning, so
  an <em|encoding> file in <verbatim|$TEXMACS_PATH/fonts/enc> maps each
  position to a <TeXmacs> character name. Those files are simply lists of
  names with the position where they start, and a font rule says which one
  a font uses.

  <paragraph*|From a file to ink>

  <name|FreeType> opens the file and gives back an outline, which is
  rasterized at the size and the resolution in use; the resulting bitmaps
  are kept in memory, per size and per resolution, for the session (the
  directory <verbatim|$TEXMACS_HOME_PATH/fonts> holds the font database and
  the bitmaps and metrics of the <TeX> fonts, not those of <name|FreeType>).
  When a document is exported to
  <name|PDF> or <name|PostScript> the same glyphs are written as vectors and
  the fonts are embedded as subsets. The glyphs which <TeXmacs> builds out
  of other glyphs are written as vectors too, except those whose
  construction works on pixels, a few emulated symbols and alphabets, which
  become small bitmap fonts.

  <paragraph*|Seeing the choices>

  The ladder is a strength: a document can use any font, and whatever the
  font lacks is found elsewhere or emulated. <menu|Tools|Fonts|Font
  inspector> opens a window which shows what it decided, and gives access
  to the other tools; none of them slows the typesetter down when it is not
  in use. The window stays above the editor windows.

  <\description>
    <item*|The inspector>Reports, as the cursor (or, with <menu|Follow the
    mouse>, the mouse) moves, what the font system did with the glyph before
    the cursor: its route, the font file which draws it, the rewriting into
    another character (a letter into its mathematical italic, for instance),
    and whether the <name|OpenType> math path applies. It reads the routing
    tables and resolves nothing, so inspecting a glyph changes nothing.
    <menu|Freeze> keeps the report of one glyph while the cursor moves on.
    While a font report or another auxiliary document has the focus, the
    inspector keeps showing the glyph at the cursor of the document it was
    looking at.

    <item*|<menu|Colour glyphs by origin>>Draws every glyph in the colour of
    its route: as usual for the requested font, blue for a family which a
    font rule names, orange for another family found by feature distance,
    green for an emulation (a virtual or a derived font); the error font is
    red anyway. Only the drawing changes, so the switch takes effect at the
    next repaint, and it is turned off with the inspector. The same switch
    is <verbatim|fonts> in the <menu|Debug> menu.

    <item*|<menu|Font report>>A document attached to the document being
    edited, like
    the bibliography viewer, which lists every character of the typeset
    document by route: a summary per family, then the emulated characters,
    those taken from other families or from families of a font rule, and
    those no font has, each shown and counted. For an emulated character it
    also says whether a <name|PDF> export will draw it as vectors or as a
    bitmap. Opening it again regenerates it.
  </description>

  Glyphs which <TeX> fonts assemble themselves, inside the compound fonts of
  the traditional mathematics, are not marked: the tools see the routes of
  the smart font, not the insides of those fonts.

  <paragraph*|When something looks wrong>

  <\description>
    <item*|A character appears as a red name>No font was found for it. Try
    <menu|Tools|Fonts|Scan disk for fonts>, and read about the merge of the
    shipped database in <hlink|the font configuration
    files|font-config.en.tm>.

    <item*|A font you installed is not proposed>The database was written
    before you installed it; the same scan adds it.

    <item*|A font looks like an older version of itself>The file found for a
    font name is cached; <menu|Tools|Fonts|Clear font cache> removes that
    cache (<verbatim|system/cache/font_cache.scm>) and the local database
    files (<verbatim|font-database.scm>, <verbatim|font-features.scm>,
    <verbatim|font-characteristics.scm> and <verbatim|shipped-stamp.scm>
    under <verbatim|fonts>), which are rebuilt at the next start.

    <item*|The document looks different on another machine>The fonts it
    asks for are not installed there, and the closest matches were used
    instead. Fonts shipped with <TeXmacs> do not have this problem.
  </description>

  <tmdoc-copyright|2026|Massimiliano Gubinelli>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>
