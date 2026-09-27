<TeXmacs|2.1.5>

<style|tmdoc>

<\body>
  <tmdoc-title|The fonts which come with <TeXmacs>>

  The following fonts are installed with <TeXmacs>, so a document which uses
  them looks the same on every system. Each is presented with the name of
  its entry in the font menu, the fonts it combines, the <LaTeX> package
  which gives the same look, and a sample set in the font itself: a line of
  text, then a formula with an integral, a sum, a fraction, a radical and
  large delimiters, then Greek letters, blackboard bold, calligraphic and
  fraktur letters, bold, sans serif and typewriter letters, a wide accent
  and a matrix. The table at the end sums up the characteristics of each
  font.

  <section|Serif text and mathematics>

  <paragraph*|Latin Modern>

  The <name|OpenType> version of Computer Modern, the font of <TeX> and of most mathematical papers. It is the default of <LaTeX> with <verbatim|unicode-math>, and the closest <name|OpenType> relative of the default <TeXmacs> font. Text: Latin Modern Roman; mathematics: Latin Modern Math; in <LaTeX>: <verbatim|lmodern>.

  <\with|font|Latin Modern Roman>
    Text in <em|italic>, <strong|bold>, <samp|sans serif> and <verbatim|typewriter>: the quick brown fox jumps over the lazy dog, 0123456789.

    <\equation*>
      <big|int><rsub|0><rsup|\<infty\>>e<rsup|-x<rsup|2>>*\<mathd\>x=<frac|<sqrt|\<pi\>>|2>,<space|2em><big|sum><rsub|n=1><rsup|\<infty\>><frac|1|n<rsup|2>>=<frac|\<pi\><rsup|2>|6>,<space|2em><around*|(|<frac|\<partial\>f|\<partial\>x>|)><rsup|2>\<leqslant\><around*|\<\|\>|f|\<\|\>><rsub|\<infty\>><rsup|2>
    </equation*>

    <\equation*>
      \<alpha\>\<beta\>\<gamma\>\<Gamma\>\<Omega\>,<space|1em>\<bbb-R\><rsup|n>,<space|1em>\<cal-F\>,<space|1em>\<frak-g\>,<space|1em><math-bf|v>+<math-ss|A>+<math-tt|x>,<space|1em><wide|x+y|^>,<space|1em><matrix|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|c>|<cell|d>>>>>
    </equation*>
  </with>

  <paragraph*|New Computer Modern>

  Computer Modern once more, extended to every mathematical symbol of <name|Unicode>, with a real bold mathematical font. The choice when a document needs rare symbols in the Computer Modern style. Text: New Computer Modern 10; mathematics: New Computer Modern Math; in <LaTeX>: <verbatim|newcomputermodern>.

  <\with|font|NewComputerModern10>
    Text in <em|italic>, <strong|bold>, <samp|sans serif> and <verbatim|typewriter>: the quick brown fox jumps over the lazy dog, 0123456789.

    <\equation*>
      <big|int><rsub|0><rsup|\<infty\>>e<rsup|-x<rsup|2>>*\<mathd\>x=<frac|<sqrt|\<pi\>>|2>,<space|2em><big|sum><rsub|n=1><rsup|\<infty\>><frac|1|n<rsup|2>>=<frac|\<pi\><rsup|2>|6>,<space|2em><around*|(|<frac|\<partial\>f|\<partial\>x>|)><rsup|2>\<leqslant\><around*|\<\|\>|f|\<\|\>><rsub|\<infty\>><rsup|2>
    </equation*>

    <\equation*>
      \<alpha\>\<beta\>\<gamma\>\<Gamma\>\<Omega\>,<space|1em>\<bbb-R\><rsup|n>,<space|1em>\<cal-F\>,<space|1em>\<frak-g\>,<space|1em><math-bf|v>+<math-ss|A>+<math-tt|x>,<space|1em><wide|x+y|^>,<space|1em><matrix|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|c>|<cell|d>>>>>
    </equation*>
  </with>

  <paragraph*|Times>

  A Times design, the font of many journals. <TeXmacs> sets its formulas with hand-tuned corrections. Text: TeX Gyre Termes; mathematics: TeX Gyre Termes Math; in <LaTeX>: <verbatim|newtx, mathptmx>.

  <\with|font|TeX Gyre Termes>
    Text in <em|italic>, <strong|bold>, <samp|sans serif> and <verbatim|typewriter>: the quick brown fox jumps over the lazy dog, 0123456789.

    <\equation*>
      <big|int><rsub|0><rsup|\<infty\>>e<rsup|-x<rsup|2>>*\<mathd\>x=<frac|<sqrt|\<pi\>>|2>,<space|2em><big|sum><rsub|n=1><rsup|\<infty\>><frac|1|n<rsup|2>>=<frac|\<pi\><rsup|2>|6>,<space|2em><around*|(|<frac|\<partial\>f|\<partial\>x>|)><rsup|2>\<leqslant\><around*|\<\|\>|f|\<\|\>><rsub|\<infty\>><rsup|2>
    </equation*>

    <\equation*>
      \<alpha\>\<beta\>\<gamma\>\<Gamma\>\<Omega\>,<space|1em>\<bbb-R\><rsup|n>,<space|1em>\<cal-F\>,<space|1em>\<frak-g\>,<space|1em><math-bf|v>+<math-ss|A>+<math-tt|x>,<space|1em><wide|x+y|^>,<space|1em><matrix|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|c>|<cell|d>>>>>
    </equation*>
  </with>

  <paragraph*|Palatino>

  A Palatino design, wider and more calligraphic than Times, popular for books and theses. Hand tuned, like Times. Text: TeX Gyre Pagella; mathematics: TeX Gyre Pagella Math; in <LaTeX>: <verbatim|newpx, mathpazo>.

  <\with|font|TeX Gyre Pagella>
    Text in <em|italic>, <strong|bold>, <samp|sans serif> and <verbatim|typewriter>: the quick brown fox jumps over the lazy dog, 0123456789.

    <\equation*>
      <big|int><rsub|0><rsup|\<infty\>>e<rsup|-x<rsup|2>>*\<mathd\>x=<frac|<sqrt|\<pi\>>|2>,<space|2em><big|sum><rsub|n=1><rsup|\<infty\>><frac|1|n<rsup|2>>=<frac|\<pi\><rsup|2>|6>,<space|2em><around*|(|<frac|\<partial\>f|\<partial\>x>|)><rsup|2>\<leqslant\><around*|\<\|\>|f|\<\|\>><rsub|\<infty\>><rsup|2>
    </equation*>

    <\equation*>
      \<alpha\>\<beta\>\<gamma\>\<Gamma\>\<Omega\>,<space|1em>\<bbb-R\><rsup|n>,<space|1em>\<cal-F\>,<space|1em>\<frak-g\>,<space|1em><math-bf|v>+<math-ss|A>+<math-tt|x>,<space|1em><wide|x+y|^>,<space|1em><matrix|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|c>|<cell|d>>>>>
    </equation*>
  </with>

  <paragraph*|Bookman>

  A Bookman design, dark and round. Hand tuned. Text: TeX Gyre Bonum; mathematics: TeX Gyre Bonum Math; in <LaTeX>: <verbatim|tgbonum>.

  <\with|font|TeX Gyre Bonum>
    Text in <em|italic>, <strong|bold>, <samp|sans serif> and <verbatim|typewriter>: the quick brown fox jumps over the lazy dog, 0123456789.

    <\equation*>
      <big|int><rsub|0><rsup|\<infty\>>e<rsup|-x<rsup|2>>*\<mathd\>x=<frac|<sqrt|\<pi\>>|2>,<space|2em><big|sum><rsub|n=1><rsup|\<infty\>><frac|1|n<rsup|2>>=<frac|\<pi\><rsup|2>|6>,<space|2em><around*|(|<frac|\<partial\>f|\<partial\>x>|)><rsup|2>\<leqslant\><around*|\<\|\>|f|\<\|\>><rsub|\<infty\>><rsup|2>
    </equation*>

    <\equation*>
      \<alpha\>\<beta\>\<gamma\>\<Gamma\>\<Omega\>,<space|1em>\<bbb-R\><rsup|n>,<space|1em>\<cal-F\>,<space|1em>\<frak-g\>,<space|1em><math-bf|v>+<math-ss|A>+<math-tt|x>,<space|1em><wide|x+y|^>,<space|1em><matrix|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|c>|<cell|d>>>>>
    </equation*>
  </with>

  <paragraph*|Schoolbook>

  A Century Schoolbook design, made for legibility. Hand tuned. Text: TeX Gyre Schola; mathematics: TeX Gyre Schola Math; in <LaTeX>: <verbatim|fouriernc, tgschola>.

  <\with|font|TeX Gyre Schola>
    Text in <em|italic>, <strong|bold>, <samp|sans serif> and <verbatim|typewriter>: the quick brown fox jumps over the lazy dog, 0123456789.

    <\equation*>
      <big|int><rsub|0><rsup|\<infty\>>e<rsup|-x<rsup|2>>*\<mathd\>x=<frac|<sqrt|\<pi\>>|2>,<space|2em><big|sum><rsub|n=1><rsup|\<infty\>><frac|1|n<rsup|2>>=<frac|\<pi\><rsup|2>|6>,<space|2em><around*|(|<frac|\<partial\>f|\<partial\>x>|)><rsup|2>\<leqslant\><around*|\<\|\>|f|\<\|\>><rsub|\<infty\>><rsup|2>
    </equation*>

    <\equation*>
      \<alpha\>\<beta\>\<gamma\>\<Gamma\>\<Omega\>,<space|1em>\<bbb-R\><rsup|n>,<space|1em>\<cal-F\>,<space|1em>\<frak-g\>,<space|1em><math-bf|v>+<math-ss|A>+<math-tt|x>,<space|1em><wide|x+y|^>,<space|1em><matrix|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|c>|<cell|d>>>>>
    </equation*>
  </with>

  <paragraph*|STIX Two>

  The font of the scientific publishers of the STIX project, a Times-like design redrawn in 2016, with the most complete set of symbols of all. It replaces the first STIX fonts, which <TeXmacs> still offers as <em|Stix>. Text: STIX Two Text; mathematics: STIX Two Math; in <LaTeX>: <verbatim|stix2>.

  <\with|font|Stix Two Text>
    Text in <em|italic>, <strong|bold>, <samp|sans serif> and <verbatim|typewriter>: the quick brown fox jumps over the lazy dog, 0123456789.

    <\equation*>
      <big|int><rsub|0><rsup|\<infty\>>e<rsup|-x<rsup|2>>*\<mathd\>x=<frac|<sqrt|\<pi\>>|2>,<space|2em><big|sum><rsub|n=1><rsup|\<infty\>><frac|1|n<rsup|2>>=<frac|\<pi\><rsup|2>|6>,<space|2em><around*|(|<frac|\<partial\>f|\<partial\>x>|)><rsup|2>\<leqslant\><around*|\<\|\>|f|\<\|\>><rsub|\<infty\>><rsup|2>
    </equation*>

    <\equation*>
      \<alpha\>\<beta\>\<gamma\>\<Gamma\>\<Omega\>,<space|1em>\<bbb-R\><rsup|n>,<space|1em>\<cal-F\>,<space|1em>\<frak-g\>,<space|1em><math-bf|v>+<math-ss|A>+<math-tt|x>,<space|1em><wide|x+y|^>,<space|1em><matrix|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|c>|<cell|d>>>>>
    </equation*>
  </with>

  <paragraph*|Libertinus>

  The maintained successor of Linux Libertine, the house font of the <name|ACM>: a warm, slightly condensed serif. Its delimiters have fewer sizes than those of the other fonts. Text: Libertinus Serif; mathematics: Libertinus Math; in <LaTeX>: <verbatim|libertine, acmart>.

  <\with|font|Libertinus>
    Text in <em|italic>, <strong|bold>, <samp|sans serif> and <verbatim|typewriter>: the quick brown fox jumps over the lazy dog, 0123456789.

    <\equation*>
      <big|int><rsub|0><rsup|\<infty\>>e<rsup|-x<rsup|2>>*\<mathd\>x=<frac|<sqrt|\<pi\>>|2>,<space|2em><big|sum><rsub|n=1><rsup|\<infty\>><frac|1|n<rsup|2>>=<frac|\<pi\><rsup|2>|6>,<space|2em><around*|(|<frac|\<partial\>f|\<partial\>x>|)><rsup|2>\<leqslant\><around*|\<\|\>|f|\<\|\>><rsub|\<infty\>><rsup|2>
    </equation*>

    <\equation*>
      \<alpha\>\<beta\>\<gamma\>\<Gamma\>\<Omega\>,<space|1em>\<bbb-R\><rsup|n>,<space|1em>\<cal-F\>,<space|1em>\<frak-g\>,<space|1em><math-bf|v>+<math-ss|A>+<math-tt|x>,<space|1em><wide|x+y|^>,<space|1em><matrix|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|c>|<cell|d>>>>>
    </equation*>
  </with>

  <paragraph*|Kp Fonts>

  A complete family inspired by the French typefaces of the Imprimerie nationale, with a light colour and many alternates. Text: KpRoman; mathematics: KpMath; in <LaTeX>: <verbatim|kpfonts>.

  <\with|font|Kepler>
    Text in <em|italic>, <strong|bold>, <samp|sans serif> and <verbatim|typewriter>: the quick brown fox jumps over the lazy dog, 0123456789.

    <\equation*>
      <big|int><rsub|0><rsup|\<infty\>>e<rsup|-x<rsup|2>>*\<mathd\>x=<frac|<sqrt|\<pi\>>|2>,<space|2em><big|sum><rsub|n=1><rsup|\<infty\>><frac|1|n<rsup|2>>=<frac|\<pi\><rsup|2>|6>,<space|2em><around*|(|<frac|\<partial\>f|\<partial\>x>|)><rsup|2>\<leqslant\><around*|\<\|\>|f|\<\|\>><rsub|\<infty\>><rsup|2>
    </equation*>

    <\equation*>
      \<alpha\>\<beta\>\<gamma\>\<Gamma\>\<Omega\>,<space|1em>\<bbb-R\><rsup|n>,<space|1em>\<cal-F\>,<space|1em>\<frak-g\>,<space|1em><math-bf|v>+<math-ss|A>+<math-tt|x>,<space|1em><wide|x+y|^>,<space|1em><matrix|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|c>|<cell|d>>>>>
    </equation*>
  </with>

  <paragraph*|Utopia>

  An extended Utopia, a crisp transitional design. Text: Erewhon; mathematics: Erewhon Math; in <LaTeX>: <verbatim|erewhon, fourier>.

  <\with|font|Erewhon>
    Text in <em|italic>, <strong|bold>, <samp|sans serif> and <verbatim|typewriter>: the quick brown fox jumps over the lazy dog, 0123456789.

    <\equation*>
      <big|int><rsub|0><rsup|\<infty\>>e<rsup|-x<rsup|2>>*\<mathd\>x=<frac|<sqrt|\<pi\>>|2>,<space|2em><big|sum><rsub|n=1><rsup|\<infty\>><frac|1|n<rsup|2>>=<frac|\<pi\><rsup|2>|6>,<space|2em><around*|(|<frac|\<partial\>f|\<partial\>x>|)><rsup|2>\<leqslant\><around*|\<\|\>|f|\<\|\>><rsub|\<infty\>><rsup|2>
    </equation*>

    <\equation*>
      \<alpha\>\<beta\>\<gamma\>\<Gamma\>\<Omega\>,<space|1em>\<bbb-R\><rsup|n>,<space|1em>\<cal-F\>,<space|1em>\<frak-g\>,<space|1em><math-bf|v>+<math-ss|A>+<math-tt|x>,<space|1em><wide|x+y|^>,<space|1em><matrix|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|c>|<cell|d>>>>>
    </equation*>
  </with>

  <paragraph*|Charter>

  An extended Bitstream Charter, sturdy and open, good at small sizes and on the screen. Text: XCharter; mathematics: XCharter Math; in <LaTeX>: <verbatim|XCharter>.

  <\with|font|XCharter>
    Text in <em|italic>, <strong|bold>, <samp|sans serif> and <verbatim|typewriter>: the quick brown fox jumps over the lazy dog, 0123456789.

    <\equation*>
      <big|int><rsub|0><rsup|\<infty\>>e<rsup|-x<rsup|2>>*\<mathd\>x=<frac|<sqrt|\<pi\>>|2>,<space|2em><big|sum><rsub|n=1><rsup|\<infty\>><frac|1|n<rsup|2>>=<frac|\<pi\><rsup|2>|6>,<space|2em><around*|(|<frac|\<partial\>f|\<partial\>x>|)><rsup|2>\<leqslant\><around*|\<\|\>|f|\<\|\>><rsub|\<infty\>><rsup|2>
    </equation*>

    <\equation*>
      \<alpha\>\<beta\>\<gamma\>\<Gamma\>\<Omega\>,<space|1em>\<bbb-R\><rsup|n>,<space|1em>\<cal-F\>,<space|1em>\<frak-g\>,<space|1em><math-bf|v>+<math-ss|A>+<math-tt|x>,<space|1em><wide|x+y|^>,<space|1em><matrix|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|c>|<cell|d>>>>>
    </equation*>
  </with>

  <paragraph*|Euler>

  Hermann Zapf's upright mathematical alphabet, designed for Knuth's <em|Concrete Mathematics>. Its letters are upright, as in handwriting; it is set with Palatino text, as with the <verbatim|eulervm> package. Text: TeX Gyre Pagella; mathematics: Euler Math; in <LaTeX>: <verbatim|eulervm>.

  <\with|font|math=Euler Math,TeX Gyre Pagella>
    Text in <em|italic>, <strong|bold>, <samp|sans serif> and <verbatim|typewriter>: the quick brown fox jumps over the lazy dog, 0123456789.

    <\equation*>
      <big|int><rsub|0><rsup|\<infty\>>e<rsup|-x<rsup|2>>*\<mathd\>x=<frac|<sqrt|\<pi\>>|2>,<space|2em><big|sum><rsub|n=1><rsup|\<infty\>><frac|1|n<rsup|2>>=<frac|\<pi\><rsup|2>|6>,<space|2em><around*|(|<frac|\<partial\>f|\<partial\>x>|)><rsup|2>\<leqslant\><around*|\<\|\>|f|\<\|\>><rsub|\<infty\>><rsup|2>
    </equation*>

    <\equation*>
      \<alpha\>\<beta\>\<gamma\>\<Gamma\>\<Omega\>,<space|1em>\<bbb-R\><rsup|n>,<space|1em>\<cal-F\>,<space|1em>\<frak-g\>,<space|1em><math-bf|v>+<math-ss|A>+<math-tt|x>,<space|1em><wide|x+y|^>,<space|1em><matrix|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|c>|<cell|d>>>>>
    </equation*>
  </with>

  <paragraph*|Concrete>

  Knuth's Concrete Roman, an even, low-contrast Computer Modern, with its mathematics; the text of <em|Concrete Mathematics> without the Euler letters. Text: CMU Concrete; mathematics: Concrete Math; in <LaTeX>: <verbatim|concmath, ccfonts>.

  <\with|font|CMU Concrete>
    Text in <em|italic>, <strong|bold>, <samp|sans serif> and <verbatim|typewriter>: the quick brown fox jumps over the lazy dog, 0123456789.

    <\equation*>
      <big|int><rsub|0><rsup|\<infty\>>e<rsup|-x<rsup|2>>*\<mathd\>x=<frac|<sqrt|\<pi\>>|2>,<space|2em><big|sum><rsub|n=1><rsup|\<infty\>><frac|1|n<rsup|2>>=<frac|\<pi\><rsup|2>|6>,<space|2em><around*|(|<frac|\<partial\>f|\<partial\>x>|)><rsup|2>\<leqslant\><around*|\<\|\>|f|\<\|\>><rsub|\<infty\>><rsup|2>
    </equation*>

    <\equation*>
      \<alpha\>\<beta\>\<gamma\>\<Gamma\>\<Omega\>,<space|1em>\<bbb-R\><rsup|n>,<space|1em>\<cal-F\>,<space|1em>\<frak-g\>,<space|1em><math-bf|v>+<math-ss|A>+<math-tt|x>,<space|1em><wide|x+y|^>,<space|1em><matrix|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|c>|<cell|d>>>>>
    </equation*>
  </with>

  <section|Sans serif text and mathematics>

  Sans serif fonts are mostly used for slides and posters, where they read
  better from a distance.

  <paragraph*|Fira>

  Mozilla's humanist sans serif and its mathematical companion, the usual choice for slides, as in the <verbatim|metropolis> theme of Beamer. The font is recent and covers fewer symbols and alphabets; <TeXmacs> emulates the missing ones from other fonts. Text: Fira Sans; mathematics: Fira Math; in <LaTeX>: <verbatim|unicode-math with Fira Math>.

  <\with|font|Fira>
    Text in <em|italic>, <strong|bold>, <samp|sans serif> and <verbatim|typewriter>: the quick brown fox jumps over the lazy dog, 0123456789.

    <\equation*>
      <big|int><rsub|0><rsup|\<infty\>>e<rsup|-x<rsup|2>>*\<mathd\>x=<frac|<sqrt|\<pi\>>|2>,<space|2em><big|sum><rsub|n=1><rsup|\<infty\>><frac|1|n<rsup|2>>=<frac|\<pi\><rsup|2>|6>,<space|2em><around*|(|<frac|\<partial\>f|\<partial\>x>|)><rsup|2>\<leqslant\><around*|\<\|\>|f|\<\|\>><rsub|\<infty\>><rsup|2>
    </equation*>

    <\equation*>
      \<alpha\>\<beta\>\<gamma\>\<Gamma\>\<Omega\>,<space|1em>\<bbb-R\><rsup|n>,<space|1em>\<cal-F\>,<space|1em>\<frak-g\>,<space|1em><math-bf|v>+<math-ss|A>+<math-tt|x>,<space|1em><wide|x+y|^>,<space|1em><matrix|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|c>|<cell|d>>>>>
    </equation*>
  </with>

  <paragraph*|Kp Sans>

  The sans serif companion of the Kp Fonts, with a sans serif mathematical font of its own and a bold one. Text: KpSans; mathematics: KpMath Sans; in <LaTeX>: <verbatim|kpfonts with sfmath>.

  <\with|font|math=KpMathSans,Kepler|font-family|ss>
    Text in <em|italic>, <strong|bold>, <samp|sans serif> and <verbatim|typewriter>: the quick brown fox jumps over the lazy dog, 0123456789.

    <\equation*>
      <big|int><rsub|0><rsup|\<infty\>>e<rsup|-x<rsup|2>>*\<mathd\>x=<frac|<sqrt|\<pi\>>|2>,<space|2em><big|sum><rsub|n=1><rsup|\<infty\>><frac|1|n<rsup|2>>=<frac|\<pi\><rsup|2>|6>,<space|2em><around*|(|<frac|\<partial\>f|\<partial\>x>|)><rsup|2>\<leqslant\><around*|\<\|\>|f|\<\|\>><rsub|\<infty\>><rsup|2>
    </equation*>

    <\equation*>
      \<alpha\>\<beta\>\<gamma\>\<Gamma\>\<Omega\>,<space|1em>\<bbb-R\><rsup|n>,<space|1em>\<cal-F\>,<space|1em>\<frak-g\>,<space|1em><math-bf|v>+<math-ss|A>+<math-tt|x>,<space|1em><wide|x+y|^>,<space|1em><matrix|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|c>|<cell|d>>>>>
    </equation*>
  </with>

  <section|The traditional fonts>

  These are the fonts of the section <menu|Text and mathematics> of the font
  menu, which do not use an <name|OpenType> mathematical font.

  <paragraph*|Roman>

  The default font of <TeXmacs>, Knuth's Computer Modern with the mathematical fonts of <TeX>.

  <\with|font|roman>
    Text in <em|italic>, <strong|bold>, <samp|sans serif> and <verbatim|typewriter>: the quick brown fox jumps over the lazy dog, 0123456789.

    <\equation*>
      <big|int><rsub|0><rsup|\<infty\>>e<rsup|-x<rsup|2>>*\<mathd\>x=<frac|<sqrt|\<pi\>>|2>,<space|2em><big|sum><rsub|n=1><rsup|\<infty\>><frac|1|n<rsup|2>>=<frac|\<pi\><rsup|2>|6>,<space|2em><around*|(|<frac|\<partial\>f|\<partial\>x>|)><rsup|2>\<leqslant\><around*|\<\|\>|f|\<\|\>><rsub|\<infty\>><rsup|2>
    </equation*>

    <\equation*>
      \<alpha\>\<beta\>\<gamma\>\<Gamma\>\<Omega\>,<space|1em>\<bbb-R\><rsup|n>,<space|1em>\<cal-F\>,<space|1em>\<frak-g\>,<space|1em><math-bf|v>+<math-ss|A>+<math-tt|x>,<space|1em><wide|x+y|^>,<space|1em><matrix|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|c>|<cell|d>>>>>
    </equation*>
  </with>

  <paragraph*|Stix>

  The first version of the STIX fonts, with hand-tuned corrections, kept for the documents which use it; STIX Two replaces it.

  <\with|font|Stix>
    Text in <em|italic>, <strong|bold>, <samp|sans serif> and <verbatim|typewriter>: the quick brown fox jumps over the lazy dog, 0123456789.

    <\equation*>
      <big|int><rsub|0><rsup|\<infty\>>e<rsup|-x<rsup|2>>*\<mathd\>x=<frac|<sqrt|\<pi\>>|2>,<space|2em><big|sum><rsub|n=1><rsup|\<infty\>><frac|1|n<rsup|2>>=<frac|\<pi\><rsup|2>|6>,<space|2em><around*|(|<frac|\<partial\>f|\<partial\>x>|)><rsup|2>\<leqslant\><around*|\<\|\>|f|\<\|\>><rsub|\<infty\>><rsup|2>
    </equation*>

    <\equation*>
      \<alpha\>\<beta\>\<gamma\>\<Gamma\>\<Omega\>,<space|1em>\<bbb-R\><rsup|n>,<space|1em>\<cal-F\>,<space|1em>\<frak-g\>,<space|1em><math-bf|v>+<math-ss|A>+<math-tt|x>,<space|1em><wide|x+y|^>,<space|1em><matrix|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|c>|<cell|d>>>>>
    </equation*>
  </with>

  <section|Characteristics>

  How complete a mathematical font is matters when a document uses rare
  symbols or unusual alphabets: what the font lacks, <TeXmacs> takes from
  another font or composes, and the result may not match the design
  perfectly. Every font below has all the symbols of everyday mathematics.

  <\big-table|<tabular|<tformat|<cwith|1|1|1|-1|cell-bborder|1ln>|<cwith|1|-1|1|-1|cell-lsep|0.5em>|<cwith|1|-1|1|-1|cell-rsep|0.5em>|<table|<row|<cell|<em|font>>|<cell|<em|symbols>>|<cell|<em|missing alphabets>>|<cell|<em|bold>>>|<row|<cell|Latin Modern>|<cell|65%>|<cell|lowercase script>|<cell|no>>|<row|<cell|New Computer Modern>|<cell|100%>|<cell|none>|<cell|yes>>|<row|<cell|Times>|<cell|67%>|<cell|none>|<cell|no>>|<row|<cell|Palatino>|<cell|67%>|<cell|none>|<cell|no>>|<row|<cell|Bookman>|<cell|67%>|<cell|none>|<cell|no>>|<row|<cell|Schoolbook>|<cell|67%>|<cell|none>|<cell|no>>|<row|<cell|STIX Two>|<cell|100%>|<cell|none>|<cell|no>>|<row|<cell|Libertinus>|<cell|67%>|<cell|none>|<cell|no>>|<row|<cell|Kp Fonts>|<cell|65%>|<cell|lowercase script, blackboard>|<cell|yes>>|<row|<cell|Utopia>|<cell|68%>|<cell|lowercase script>|<cell|no>>|<row|<cell|Charter>|<cell|67%>|<cell|lowercase script>|<cell|yes>>|<row|<cell|Euler>|<cell|66%>|<cell|lowercase script>|<cell|no>>|<row|<cell|Concrete>|<cell|67%>|<cell|lowercase script>|<cell|yes>>|<row|<cell|Fira>|<cell|43%>|<cell|script, fraktur, sans serif>|<cell|no>>|<row|<cell|Kp Sans>|<cell|63%>|<cell|lowercase script, blackboard>|<cell|yes>>>>>>
    How complete the mathematical fonts which come with <TeXmacs> are. <em|Symbols> is the part of the list of symbols of the <LaTeX> package <verbatim|unicode-math> which the font has; <em|missing alphabets> names the mathematical alphabets of <name|Unicode> (bold, italic, script, fraktur, blackboard bold, sans serif, typewriter) which are incomplete, and which <TeXmacs> emulates; <em|bold> says whether there is a bold mathematical font.
  </big-table>

  <\big-table|<tabular|<tformat|<cwith|1|1|1|-1|cell-bborder|1ln>|<cwith|1|-1|1|-1|cell-lsep|0.5em>|<cwith|1|-1|1|-1|cell-rsep|0.5em>|<table|<row|<cell|<em|font>>|<cell|<em|sans serif, typewriter>>|<cell|<em|license>>>|<row|<cell|Latin Modern>|<cell|LM Sans, LM Mono>|<cell|GUST>>|<row|<cell|New Computer Modern>|<cell|none>|<cell|GUST>>|<row|<cell|Times>|<cell|Heros, Cursor>|<cell|GUST>>|<row|<cell|Palatino>|<cell|Heros, Cursor>|<cell|GUST>>|<row|<cell|Bookman>|<cell|Adventor, Cursor>|<cell|GUST>>|<row|<cell|Schoolbook>|<cell|Heros, Cursor>|<cell|GUST>>|<row|<cell|STIX Two>|<cell|none>|<cell|OFL>>|<row|<cell|Libertinus>|<cell|Libertinus Sans, Mono>|<cell|OFL>>|<row|<cell|Kp Fonts>|<cell|KpSans, KpMono>|<cell|OFL>>|<row|<cell|Utopia>|<cell|none>|<cell|OFL>>|<row|<cell|Charter>|<cell|none>|<cell|OFL, Bitstream>>|<row|<cell|Euler>|<cell|Heros, Cursor>|<cell|OFL>>|<row|<cell|Concrete>|<cell|none>|<cell|OFL>>|<row|<cell|Fira>|<cell|Fira Mono>|<cell|OFL>>|<row|<cell|Kp Sans>|<cell|KpMono>|<cell|OFL>>>>>>
    The sans serif and typewriter companions which come with <TeXmacs>, and the license of each font: the GUST Font License or the SIL Open Font License (OFL).
  </big-table>

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
