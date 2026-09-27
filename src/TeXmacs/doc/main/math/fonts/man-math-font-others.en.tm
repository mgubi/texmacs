<TeXmacs|2.1.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Other mathematical fonts>

  <TeXmacs> knows more mathematical fonts than it installs. The fonts of
  this page are used as soon as they are installed on your system: they
  then appear in the font menu like the fonts which come with <TeXmacs>,
  with their text companions. Most of them are part of <TeX> Live, the
  <TeX> distribution, which <TeXmacs> searches for fonts; they are all free.

  Each font is shown with a sample which is set in the font itself when it is
  installed. When it is not, <TeXmacs> sets the sample in the closest font it
  can find, which is the situation of a document that asks for a font your
  system does not have.

  <section|Fonts <TeXmacs> knows>

  <paragraph*|DejaVu>

  A mathematical companion of the DejaVu fonts, wide and very legible on the screen, in the style of the <name|TeX Gyre> fonts. Text: DejaVu Serif; mathematics: TeX Gyre DejaVu Math, with 67% of the symbols. Where to find it: <verbatim|tex-gyre-math> and <verbatim|dejavu> in <TeX> Live; the DejaVu fonts come with most <name|Linux> systems. Once installed, it appears in the section <menu|Serif text and mathematics> of the font menu.

  <\with|font|DejaVu>
    Text in <em|italic> and <strong|bold>: the quick brown fox, 0123456789.

    <\equation*>
      <big|int><rsub|0><rsup|\<infty\>>e<rsup|-x<rsup|2>>*\<mathd\>x=<frac|<sqrt|\<pi\>>|2>,<space|2em><big|sum><rsub|n=1><rsup|\<infty\>><frac|1|n<rsup|2>>=<frac|\<pi\><rsup|2>|6>,<space|2em>\<alpha\>\<beta\>\<gamma\>,<space|1em>\<bbb-R\><rsup|n>,<space|1em>\<cal-F\>
    </equation*>
  </with>

  <paragraph*|Computer Modern Sans>

  The sans serif Computer Modern of the Beamer default, with a sans serif mathematical font of its own. Text: New Computer Modern Sans; mathematics: New Computer Modern Sans Math, with all of the symbols. Where to find it: <verbatim|newcomputermodern> in <TeX> Live. Once installed, it appears in the section <menu|Sans serif text and mathematics> of the font menu.

  <\with|font|NewComputerModernSans10>
    Text in <em|italic> and <strong|bold>: the quick brown fox, 0123456789.

    <\equation*>
      <big|int><rsub|0><rsup|\<infty\>>e<rsup|-x<rsup|2>>*\<mathd\>x=<frac|<sqrt|\<pi\>>|2>,<space|2em><big|sum><rsub|n=1><rsup|\<infty\>><frac|1|n<rsup|2>>=<frac|\<pi\><rsup|2>|6>,<space|2em>\<alpha\>\<beta\>\<gamma\>,<space|1em>\<bbb-R\><rsup|n>,<space|1em>\<cal-F\>
    </equation*>
  </with>

  <paragraph*|Lete Sans>

  A sans serif mathematical font designed to go with Lato; <TeXmacs> sets the text in the letters of the mathematical font itself. Text: Lete Sans Math itself; mathematics: Lete Sans Math, with 95% of the symbols. Where to find it: <verbatim|lete-sans-math> in <TeX> Live. Once installed, it appears in the section <menu|Sans serif text and mathematics> of the font menu.

  <\with|font|Lete Sans Math>
    Text in <em|italic> and <strong|bold>: the quick brown fox, 0123456789.

    <\equation*>
      <big|int><rsub|0><rsup|\<infty\>>e<rsup|-x<rsup|2>>*\<mathd\>x=<frac|<sqrt|\<pi\>>|2>,<space|2em><big|sum><rsub|n=1><rsup|\<infty\>><frac|1|n<rsup|2>>=<frac|\<pi\><rsup|2>|6>,<space|2em>\<alpha\>\<beta\>\<gamma\>,<space|1em>\<bbb-R\><rsup|n>,<space|1em>\<cal-F\>
    </equation*>
  </with>

  <paragraph*|XITS>

  A Times design derived from the first STIX fonts, with a bold mathematical font and right to left mathematics; STIX Two, which comes with <TeXmacs>, has superseded it. Text: XITS; mathematics: XITS Math, with 99% of the symbols. Where to find it: <verbatim|xits> in <TeX> Live. Once installed, it appears in the submenu <menu|Other OpenType math fonts> of the font menu.

  <\with|font|XITS Math>
    Text in <em|italic> and <strong|bold>: the quick brown fox, 0123456789.

    <\equation*>
      <big|int><rsub|0><rsup|\<infty\>>e<rsup|-x<rsup|2>>*\<mathd\>x=<frac|<sqrt|\<pi\>>|2>,<space|2em><big|sum><rsub|n=1><rsup|\<infty\>><frac|1|n<rsup|2>>=<frac|\<pi\><rsup|2>|6>,<space|2em>\<alpha\>\<beta\>\<gamma\>,<space|1em>\<bbb-R\><rsup|n>,<space|1em>\<cal-F\>
    </equation*>
  </with>

  <paragraph*|Asana>

  A Palatino design with a larger set of symbols than Pagella Math; it is set with Pagella text. Text: TeX Gyre Pagella; mathematics: Asana Math, with 94% of the symbols. Where to find it: <verbatim|asana-math> in <TeX> Live. Once installed, it appears in the submenu <menu|Other OpenType math fonts> of the font menu.

  <\with|font|math=Asana Math,TeX Gyre Pagella>
    Text in <em|italic> and <strong|bold>: the quick brown fox, 0123456789.

    <\equation*>
      <big|int><rsub|0><rsup|\<infty\>>e<rsup|-x<rsup|2>>*\<mathd\>x=<frac|<sqrt|\<pi\>>|2>,<space|2em><big|sum><rsub|n=1><rsup|\<infty\>><frac|1|n<rsup|2>>=<frac|\<pi\><rsup|2>|6>,<space|2em>\<alpha\>\<beta\>\<gamma\>,<space|1em>\<bbb-R\><rsup|n>,<space|1em>\<cal-F\>
    </equation*>
  </with>

  <paragraph*|IBM Plex>

  IBM's corporate family, with serif, sans serif and typewriter faces and a complete mathematical font of recent design. Text: IBM Plex Serif; mathematics: IBM Plex Math, with 99% of the symbols. Where to find it: <verbatim|plex> and <verbatim|plex-otf> in <TeX> Live, or from IBM. Once installed, it appears in the submenu <menu|Other OpenType math fonts> of the font menu.

  <\with|font|IBM Plex Math>
    Text in <em|italic> and <strong|bold>: the quick brown fox, 0123456789.

    <\equation*>
      <big|int><rsub|0><rsup|\<infty\>>e<rsup|-x<rsup|2>>*\<mathd\>x=<frac|<sqrt|\<pi\>>|2>,<space|2em><big|sum><rsub|n=1><rsup|\<infty\>><frac|1|n<rsup|2>>=<frac|\<pi\><rsup|2>|6>,<space|2em>\<alpha\>\<beta\>\<gamma\>,<space|1em>\<bbb-R\><rsup|n>,<space|1em>\<cal-F\>
    </equation*>
  </with>

  <paragraph*|Garamond>

  A Garamond for mathematics, to go with EB Garamond, the free revival of the sixteenth century design. Text: EB Garamond; mathematics: Garamond-Math, with 67% of the symbols. Where to find it: <verbatim|garamond-math> and <verbatim|ebgaramond> in <TeX> Live. Once installed, it appears in the submenu <menu|Other OpenType math fonts> of the font menu.

  <\with|font|Garamond-Math>
    Text in <em|italic> and <strong|bold>: the quick brown fox, 0123456789.

    <\equation*>
      <big|int><rsub|0><rsup|\<infty\>>e<rsup|-x<rsup|2>>*\<mathd\>x=<frac|<sqrt|\<pi\>>|2>,<space|2em><big|sum><rsub|n=1><rsup|\<infty\>><frac|1|n<rsup|2>>=<frac|\<pi\><rsup|2>|6>,<space|2em>\<alpha\>\<beta\>\<gamma\>,<space|1em>\<bbb-R\><rsup|n>,<space|1em>\<cal-F\>
    </equation*>
  </with>

  <paragraph*|Old Standard>

  A revival of the typefaces of scientific books around 1900, with a complete mathematical font; it has Greek and Cyrillic text. Text: Old Standard; mathematics: Old Standard Math, with 100% of the symbols. Where to find it: <verbatim|oldstandard> in <TeX> Live. Once installed, it appears in the submenu <menu|Other OpenType math fonts> of the font menu.

  <\with|font|OldStandard-Math>
    Text in <em|italic> and <strong|bold>: the quick brown fox, 0123456789.

    <\equation*>
      <big|int><rsub|0><rsup|\<infty\>>e<rsup|-x<rsup|2>>*\<mathd\>x=<frac|<sqrt|\<pi\>>|2>,<space|2em><big|sum><rsub|n=1><rsup|\<infty\>><frac|1|n<rsup|2>>=<frac|\<pi\><rsup|2>|6>,<space|2em>\<alpha\>\<beta\>\<gamma\>,<space|1em>\<bbb-R\><rsup|n>,<space|1em>\<cal-F\>
    </equation*>
  </with>

  <paragraph*|GFS Neohellenic>

  A Greek sans serif of the Greek Font Society with a mathematical font; most of the mathematical alphabets are missing. Text: GFS Neohellenic; mathematics: GFS Neohellenic Math, with 68% of the symbols. Where to find it: <verbatim|gfsneohellenicmath> in <TeX> Live. Once installed, it appears in the submenu <menu|Other OpenType math fonts> of the font menu.

  <\with|font|GFS Neohellenic Math>
    Text in <em|italic> and <strong|bold>: the quick brown fox, 0123456789.

    <\equation*>
      <big|int><rsub|0><rsup|\<infty\>>e<rsup|-x<rsup|2>>*\<mathd\>x=<frac|<sqrt|\<pi\>>|2>,<space|2em><big|sum><rsub|n=1><rsup|\<infty\>><frac|1|n<rsup|2>>=<frac|\<pi\><rsup|2>|6>,<space|2em>\<alpha\>\<beta\>\<gamma\>,<space|1em>\<bbb-R\><rsup|n>,<space|1em>\<cal-F\>
    </equation*>
  </with>

  <section|Other <name|OpenType> math fonts>

  Any font with an <name|OpenType> <verbatim|MATH> table can set formulas,
  even one <TeXmacs> knows nothing about: its formulas are laid out from the
  table, with no adjustment. Such a font does not appear in the font menu;
  name it in a rule of the <src-var|font> variable, followed by the text font
  to use with it, in the markup of the document

  <\tm-fragment>
    <inactive*|<assign|font|math=Cambria Math,Cambria>>
  </tm-fragment>

  The best known of these fonts are:

  <\description>
    <item*|Cambria Math>The mathematical font of <name|Microsoft Office>,
    installed with <name|Windows> and <name|Office>. It cannot be shipped
    with <TeXmacs>, but works when it is present.

    <item*|Lucida Bright Math>A commercial font, sold by the <TeX> Users
    Group, with a wide set of symbols and several weights.

    <item*|Minion Math>A commercial companion of Adobe's Minion, with
    several weights and optical sizes.
  </description>

  A font without a <verbatim|MATH> table, such as Noto Sans Math, only
  provides symbols: it cannot place scripts or build large delimiters, and
  is not a mathematical font in the above sense.

  <section|Installing fonts>

  <TeXmacs> looks for fonts in the directory <verbatim|fonts/truetype> of its
  installation and of your <TeXmacs> home directory (<verbatim|~/.TeXmacs>),
  in the font directories of your system, in the <TeX> Live installations it
  finds, and in the directories listed in the environment variable
  <verbatim|TEXMACS_FONT_PATH>. To add a font:

  <\enumerate>
    <item>Install it in the usual way for your system, with the package
    manager of <TeX> Live (<verbatim|tlmgr install xits>, for instance), or
    copy its <verbatim|.otf> files into <verbatim|~/.TeXmacs/fonts/truetype>.

    <item>Run <menu|Tools|Fonts|Scan disk for fonts>, so that <TeXmacs>
    records the new fonts. A mathematical font which <TeXmacs> knows is also
    recorded by itself the first time it is asked for.

    <item>If a font still does not appear, <menu|Tools|Fonts|Clear font
    cache> makes <TeXmacs> forget the fonts it used to find instead.
  </enumerate>

  Remember that a document which uses a font you installed will look
  different on a system which does not have it.

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
