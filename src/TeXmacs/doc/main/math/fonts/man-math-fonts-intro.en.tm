<TeXmacs|2.1.5>

<style|tmdoc>

<\body>
  <tmdoc-title|How mathematical fonts work>

  <paragraph*|Text and mathematics go together>

  A document has one main font, and its formulas are set in the
  mathematical font which goes with it. The fonts are designed in pairs: the
  letters of a formula have the same shapes, weights and proportions as the
  surrounding text, so that <math|x> in a sentence and <math|x> in a
  displayed equation look like the same letter. Changing the main font of a
  document therefore changes its mathematics too. For most fonts
  <TeXmacs> also knows a <em|sans serif> and a <em|typewriter> companion of
  the same design, which serve <samp|sans serif> and <verbatim|typewriter>
  text as well as the sans serif and typewriter letters of formulas.

  <paragraph*|Three kinds of mathematical fonts>

  <\description>
    <item*|The <TeX> fonts>The default font of <TeXmacs>, <em|Roman>, is
    Knuth's Computer Modern with the mathematical fonts of <TeX>. Its
    symbols are drawn from several <TeX> fonts, and <TeXmacs> composes the
    large delimiters and the symbols which no font provides itself. The
    older <em|Concrete> and <em|Euler new roman> are of the same kind.

    <item*|<name|OpenType> math fonts>Modern mathematical fonts carry a
    table, called <verbatim|MATH>, which describes everything a formula
    needs: the position of scripts and limits, the thickness of fraction
    bars and radicals, the size variants of every delimiter and how to build
    one of any height from pieces, the italic corrections and the placement
    of accents. <TeXmacs> reads this table and lays out formulas as the
    designer of the font intended, as <LaTeX> does with the
    <verbatim|unicode-math> package. Almost all the fonts of this section are
    of this kind.

    <item*|Hand-tuned fonts>For STIX (the first version) and the four
    <name|TeX Gyre> fonts, <TeXmacs> has its own corrections, adjusted by
    hand before it could read the <verbatim|MATH> table. They are still used,
    and the table only supplies what they do not cover. The preference
    <menu|Hand tuned math fonts>, among the experimental features of the tab
    <menu|Other> of the preferences (<menu|Edit|Preferences>, or
    <menu|TeXmacs|Preferences> on <name|macOS>), switches them off, to see
    what the table alone gives.
  </description>

  <paragraph*|Choosing the fonts of a document>

  The simplest way is the font button of the focus toolbar, which shows the
  name of the main font of the document (for instance <menu|Roman>) when the
  cursor is not inside any particular tag. Its menu sorts the fonts by
  design, in the submenus <menu|Serif>, <menu|Sans serif>,
  <menu|Typewriter> and <menu|Decorative>. The first two begin with a
  section <menu|With mathematics>: Roman and the first STIX fonts among
  the serif ones, and the
  <name|OpenType> pairs, under the names <LaTeX> users know: <menu|Times>
  for <name|TeX Gyre> Termes as with the <verbatim|newtx> package,
  <menu|Palatino> for <name|TeX Gyre> Pagella as with <verbatim|newpx>,
  <menu|Utopia>, <menu|Charter>, <menu|Euler> and so on. Fonts <TeXmacs>
  knows but which are less common are in a submenu <menu|Other OpenType math
  fonts>. Every entry sets the text font, the mathematical font and, when
  the pair calls for it, the font family, and a menu lists only the fonts
  which are installed. The second section of a submenu, <menu|Text only>,
  changes the text font alone.

  <menu|Document|Font|Mathematical font> changes the mathematical font alone,
  and keeps the text font. Its entries are the traditional <TeXmacs> math
  fonts and, at the end, the installed <name|OpenType> math fonts. Formulas
  follow the text font, so this menu only has an effect while the text font
  is the default one, Roman; to combine another text font with a
  mathematical font, use the font button, or the rule described below.

  <paragraph*|The same thing in markup>

  An entry of the menu stores its choice in the document as environment
  variables. The main font is <src-var|font>, which names a font by its
  family, <verbatim|Libertinus> or <verbatim|Kepler> for instance, and
  formulas are then set in the mathematical font which belongs to that
  family. (The <name|TeX Gyre> entries, Times, Palatino, Bookman and
  Schoolbook, add a style package instead, such as
  <verbatim|pagella-font>, which also sets the hand-tuned mathematics.) When a text font is combined with another mathematical font than
  its own, the value of <src-var|font> is a <em|rule> which names both: Euler
  is set with Pagella text by

  <\tm-fragment>
    <inactive*|<assign|font|math=Euler Math,TeX Gyre Pagella>>
  </tm-fragment>

  and the part before the comma applies to mathematics only. The variable
  <src-var|font-family> says whether the text is set in roman (<verbatim|rm>),
  sans serif (<verbatim|ss>) or typewriter (<verbatim|tt>) letters; a sans
  serif pair such as Kp Sans sets it to <verbatim|ss>. A formula, or any part
  of a document, can be given other fonts with <markup|with>:

  <\tm-fragment>
    <inactive*|<with|font|Fira|<math|a<rsup|2>+b<rsup|2>=c<rsup|2>>>>
  </tm-fragment>

  <paragraph*|Bold, sans serif and typewriter formulas>

  Bold mathematics (for instance in a bold title, or with
  <src-var|math-font-series> set to <verbatim|bold>) uses a real bold
  mathematical font when the family has one, as New Computer Modern, Kp
  Fonts, Utopia, Charter and Concrete do, and is otherwise emulated by thickening
  the strokes of the regular font. Sans serif and typewriter letters in a
  formula come from the companions of the font.

  <paragraph*|Letters, alphabets and alternates>

  With an <name|OpenType> math font, the letters of a formula are taken
  from the mathematical italic alphabet of the font, so that the italic
  corrections and the kerning of the font apply to them; the hand-tuned
  <name|TeX Gyre> fonts take them from the italic of their text font. An <name|OpenType> math font also has alphabets for
  blackboard bold (<math|\<bbb-R\>>), calligraphic (<math|\<cal-F\>>) and
  fraktur (<math|\<frak-g\>>) letters, which are used when present;
  <TeXmacs> emulates those which are missing. In scripts, the smaller
  alternates the font designs for that purpose are used, as are its dotless
  <math|i> and <math|j> under an accent. The font browser, and, when
  complex actions go through the menus, <menu|Format|Font features> and
  <menu|Document|Font|Features>, give access to the other variants a font
  offers, such as old style figures or its stylistic sets.

  <paragraph*|Fonts that are not installed>

  A document names its fonts, not files, and the name is looked up again
  every time the document is opened. On a system where a font is missing,
  <TeXmacs> uses the closest font it can find, so a document looks the same
  on every system only when it uses fonts which come with <TeXmacs>; the
  next page shows them all. After installing new fonts, <menu|Tools|Fonts|Scan
  disk for fonts> makes them known. The chapter <hlink|<em|Fonts, from
  selection to glyph>|../../../devel/fonts/font-guide.en.tm> of the reference guide
  explains the machinery in detail.

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
