<TeXmacs|2.1.5>

<style|<tuple|article|doc>>

<\body>
  <\hide-preamble>
    <assign|qs|<macro|menu|kbd|cmd|ico|<tabular|<tformat|<cwith|1|1|1|-1|cell-background|yellow>|<cwith|1|1|1|-1|cell-lborder|1ln>|<cwith|1|1|1|-1|cell-rborder|1ln>|<cwith|1|1|1|-1|cell-tborder|1ln>|<cwith|1|1|1|-1|cell-bborder|1ln>|<cwith|1|1|1|-1|cell-valign|c>|<cwith|1|1|1|1|cell-hmode|exact>|<cwith|1|1|1|1|cell-width|0.47par>|<cwith|1|1|2|2|cell-hmode|exact>|<cwith|1|1|2|2|cell-width|7.5em>|<cwith|1|1|3|3|cell-hmode|exact>|<cwith|1|1|3|3|cell-width|8.5em>|<cwith|1|1|4|4|cell-hmode|exact>|<cwith|1|1|4|4|cell-width|2.5em>|<cwith|1|1|4|4|cell-halign|c>|<table|<row|<cell|<arg|menu>>|<cell|<arg|kbd>>|<cell|<with|color|dark
    green|<arg|cmd>>>|<cell|<arg|ico>>>>>>>>

    <assign|qs-menu|<macro|menu|<tabular|<tformat|<cwith|1|1|1|1|cell-background|yellow>|<cwith|1|1|1|1|cell-lborder|1ln>|<cwith|1|1|1|1|cell-rborder|1ln>|<cwith|1|1|1|1|cell-tborder|1ln>|<cwith|1|1|1|1|cell-bborder|1ln>|<table|<row|<cell|<arg|menu>>>>>>>>

    <assign|qs-cmd|<macro|name|<verbatim|\\<arg|name>>>>
  </hide-preamble>

  <\with|par-sep|0fn>
    <qs-menu|<menu|Document|Style|article>>

    <qs-menu|<menu|Document|Language|English>>

    <qs-menu|<menu|Document|Page>, tab <em|Format>: <em|Page rendering>
    \<rightarrow\> <em|paper>, <em|Page type> \<rightarrow\> <em|a4>>
  </with>

  <vspace|0.5fn>

  <\with|par-sep|0fn>
    <qs-menu|<menu|Document|Informative flags|Detailed>>

    <qs-menu|<menu|Document|Page>, tab <em|Margins>: <em|Same screen margins
    as on paper>>
  </with>

  <qs|<menu|Insert|Title|Insert title>||<qs-cmd|maketitle>|>

  <doc-data|<doc-title|<TeXmacs> Quick-Start
  Guide>|<doc-running-title|<TeXmacs> Quick-Start
  Guide>|<doc-author|<author-data|<author-name|Nicolas
  Ratier>|<\author-affiliation>
    Institut FEMTO-ST, d\<#E9\>partement LPMO, CNRS UMR 6174

    32 avenue de l'Observatoire, F-25044 Besan\<#E7\>on, France
  </author-affiliation>|<author-email|nicolas.ratier@lpmo.edu>|<author-homepage|www.lpmo.edu/~ratier>>>|<doc-misc|Updated
  for <TeXmacs> 2.1.5 in 2026.>>

  <qs|<menu|Insert|Title|Abstract>||<qs-cmd|abstract>|>

  <\abstract>
    This quick-start guide is a summary of the <TeXmacs> commands used to
    write scientific papers. Most commands are available in four ways: from
    the menu entries, by keyboard shortcuts, by <LaTeX>-like keyboard inputs
    (type <key|\\> and the name of the command, then <key|return>) and by the
    icons of the toolbars. In the yellow boxes, these are given in this
    order. The keyboard shortcuts are shown as on <name|macOS>; on other
    systems, the menus of <TeXmacs> show the shortcuts of your system.
  </abstract>

  <qs|<menu|Insert|Section|Section>|<shortcut|(make-section
  'section)>|<qs-cmd|section>|<icon|tm_section.xpm>>

  <section|Section>

  <qs|<menu|Insert|Section|Subsection>|<shortcut|(make-section
  'subsection)>|<qs-cmd|subsection>|<icon|tm_section.xpm>>

  <subsection|Subsection>

  <qs|<menu|Insert|Section|Subsubsection>|<shortcut|(make-section
  'subsubsection)>|<qs-cmd|subsubsection>|<icon|tm_section.xpm>>

  <subsubsection|Subsubsection<label|subsubsec>>

  <qs|<menu|Insert|Mathematics|Inline formula>|<key|$>|<qs-cmd|(>|<icon|tm_math.xpm>>

  Example of inline formula: <math|sin<around*|(|x|)><rsup|2>+cos<around*|(|x|)><rsup|2>=1>.

  <\with|par-sep|0fn>
    <qs|<menu|Insert|Mathematics|Displayed
    formula>|<shortcut|(make-equation*)>|<qs-cmd|[>|<icon|tm_math.xpm>>

    <qs|<menu|Focus|Numbered> (on/off)|<shortcut|(numbered-toggle
    (focus-tree))>||<icon|tm_numbered.xpm>>
  </with>

  <\equation>
    <frac|d<rsup|2>x<rsup|\<beta\>>|d\<lambda\><rsup|2>>+\<Gamma\><rsub|\<mu\>*\<nu\>><rsup|\<beta\>>*<frac|d*x<rsup|\<mu\>>|d*\<lambda\>>*<frac|d*x<rsup|\<nu\>>|d*\<lambda\>>=0
  </equation>

  <\with|par-sep|0fn>
    <qs|<menu|Insert|Mathematics|Several equations>|<shortcut|(make-eqnarray*)>|<qs-cmd|eqnarray*>|<icon|tm_math.xpm>>

    <qs|<menu|Focus|Numbered> (on/off, per line)|<shortcut|(numbered-toggle
    (focus-tree))>||<icon|tm_numbered.xpm>>
  </with>

  <\eqnarray*>
    <tformat|<table|<row|<cell|f<rsub|m,n>>|<cell|=>|<cell|<frac|1|M*N>*<big|sum><rsub|u=0><rsup|M-1><big|sum><rsub|v=0><rsup|N-1>F<rsub|u,v>*exp<around*|[|2*i*\<pi\>*<around*|(|<frac|m*u|M>+<frac|n*v|N>|)>|]><eq-number><label|eq-fourier>>>|<row|<cell|F<rsub|u,v>>|<cell|=>|<cell|<big|sum><rsub|m=0><rsup|M-1><big|sum><rsub|n=0><rsup|N-1>f<rsub|m,n>*exp<around*|[|-2*i*\<pi\>*<around*|(|<frac|m*u|M>+<frac|n*v|N>|)>|]><eq-number><label|eq-inverse>>>>>
  </eqnarray*>

  <\with|par-sep|0fn>
    <qs|<menu|Insert|Image|Big figure>||<qs-cmd|figure>|<icon|tm_image.xpm>>

    <qs|<menu|Insert|Image|Insert image>|||<icon|tm_image.xpm>>
  </with>

  <\big-figure|<image|images/quick-start-window.png|0.6par|||>>
    Part of the <TeXmacs> Quick-Start Guide, in <TeXmacs>.<label|fig-window>
  </big-figure>

  <\with|par-sep|0fn>
    <qs|<menu|Insert|Table|Big table>||<qs-cmd|table>|<icon|tm_table.xpm>>

    <qs|<menu|Insert|Table|Plain block>|<shortcut|(make
    'block)>||<icon|tm_table.xpm>>
  </with>

  <\big-table|<block|<tformat|<table|<row|<cell|pdf>|<cell|eps>|<cell|ps>|<cell|svg>|<cell|png>|<cell|jpg>>|<row|<cell|gif>|<cell|tif>|<cell|xpm>|<cell|ppm>|<cell|pnm>|<cell|fig>>>>>>
    Image file formats currently recognized by <TeXmacs>.<label|tab-formats>
  </big-table>

  <\with|par-sep|0fn>
    <qs|<menu|Insert|Link|Label>|<shortcut|(make-label)>|<qs-cmd|label>|<icon|tm_link.xpm>>

    <qs|<menu|Insert|Link|Reference>|<key|C-?>|<qs-cmd|ref>|<icon|tm_link.xpm>>
  </with>

  Reference to subsubsection <reference|subsubsec>, to Eq.
  (<reference|eq-fourier>), to Fig. <reference|fig-window> and to Table
  <reference|tab-formats>.

  <\with|par-sep|0fn>
    <qs|<menu|Insert|Link|Citation|Visible>||<qs-cmd|cite>|<icon|tm_link.xpm>>

    <qs|<menu|Insert|Link|Citation|Invisible>||<qs-cmd|nocite>|<icon|tm_link.xpm>>
  </with>

  Reference to citation <cite|X1> and invisible citation.<nocite|X2>

  <\with|par-sep|0fn>
    <qs|<menu|Insert|Automatic|Bibliography>|||<icon|tm_index.xpm>>

    <qs|<menu|Document|Update|Bibliography>|||>
  </with>

  <\bibliography|bib|tm-plain|quick-start.bib>
    <\bib-list|2>
      <bibitem*|1><label|bib-X1>X1.<nbsp>Author. <newblock>Title of the
      paper. <newblock><with|font-shape|italic|International Journal of
      Science and Technology>, X(X):XX\UXX, 19XX.<newblock>

      <bibitem*|2><label|bib-X2>X2.<nbsp>Author.
      <newblock><with|font-shape|italic|Title of the Book>.
      <newblock>Publishing House, 19XX.<newblock>
    </bib-list>
  </bibliography>

  <\with|par-mode|right>
    \<copyright\> Nicolas Ratier 2005
  </with>
</body>

<\initial>
  <\collection>
    <associate|info-flag|detailed>
    <associate|page-medium|paper>
    <associate|page-screen-margin|false>
    <associate|page-type|a4>
    <associate|par-first|0fn>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|auto-1|<tuple|?|1>>
    <associate|auto-10|<tuple|1|1>>
    <associate|auto-11|<tuple|1.1|1>>
    <associate|auto-12|<tuple|1.1|1>>
    <associate|auto-13|<tuple|1.1.1|1>>
    <associate|auto-14|<tuple|1.1.1|1>>
    <associate|auto-15|<tuple|1.1.1|1>>
    <associate|auto-16|<tuple|1.1.1|1>>
    <associate|auto-17|<tuple|1|1>>
    <associate|auto-18|<tuple|1|1>>
    <associate|auto-19|<tuple|3|2>>
    <associate|auto-2|<tuple|?|1>>
    <associate|auto-20|<tuple|3|2>>
    <associate|auto-21|<tuple|1|2>>
    <associate|auto-22|<tuple|1|2>>
    <associate|auto-23|<tuple|1|2>>
    <associate|auto-24|<tuple|1|2>>
    <associate|auto-25|<tuple|1|2>>
    <associate|auto-26|<tuple|1|2>>
    <associate|auto-27|<tuple|1|2>>
    <associate|auto-28|<tuple|1|2>>
    <associate|auto-29|<tuple|1|2>>
    <associate|auto-3|<tuple|?|1>>
    <associate|auto-30|<tuple|1|2>>
    <associate|auto-31|<tuple|1|2>>
    <associate|auto-4|<tuple|?|1>>
    <associate|auto-5|<tuple|?|1>>
    <associate|auto-6|<tuple|?|1>>
    <associate|auto-7|<tuple|?|1>>
    <associate|auto-8|<tuple|?|1>>
    <associate|auto-9|<tuple|1|1>>
    <associate|bib-X1|<tuple|1|2>>
    <associate|bib-X2|<tuple|2|2>>
    <associate|eq-fourier|<tuple|2|1>>
    <associate|eq-inverse|<tuple|3|1>>
    <associate|fig-window|<tuple|1|2>>
    <associate|subsubsec|<tuple|1.1.1|1>>
    <associate|tab-formats|<tuple|1|2>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|bib>
      X1

      X2
    </associate>
    <\associate|figure>
      <tuple|normal|<\surround|<hidden-binding|<tuple>|1>|>
        Part of the T<rsub|<space|-0.4spc><move|<resize|<with|math-level|<quote|0>|E>||||0.5fn>|0fn|-0.1fn>><space|-0.4spc>X<rsub|<space|-0.4spc><move|<resize|M<space|-0.2spc>A<space|-0.4spc>CS||||0.5fn>|0fn|-0.1fn>>
        Quick-Start Guide, in T<rsub|<space|-0.4spc><move|<resize|<with|math-level|<quote|0>|E>||||0.5fn>|0fn|-0.1fn>><space|-0.4spc>X<rsub|<space|-0.4spc><move|<resize|M<space|-0.2spc>A<space|-0.4spc>CS||||0.5fn>|0fn|-0.1fn>>.
      </surround>|<pageref|auto-21>>
    </associate>
    <\associate|idx>
      <tuple|<tuple|<with|font-family|<quote|ss>|Document>|<with|font-family|<quote|ss>|Style>|<with|font-family|<quote|ss>|article>>|<pageref|auto-1>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Document>|<with|font-family|<quote|ss>|Language>|<with|font-family|<quote|ss>|English>>|<pageref|auto-2>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Document>|<with|font-family|<quote|ss>|Page>>|<pageref|auto-3>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Document>|<with|font-family|<quote|ss>|Informative
      flags>|<with|font-family|<quote|ss>|Detailed>>|<pageref|auto-4>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Document>|<with|font-family|<quote|ss>|Page>>|<pageref|auto-5>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Title>|<with|font-family|<quote|ss>|Insert
      title>>|<pageref|auto-6>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Title>|<with|font-family|<quote|ss>|Abstract>>|<pageref|auto-7>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Section>|<with|font-family|<quote|ss>|Section>>|<pageref|auto-8>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Section>|<with|font-family|<quote|ss>|Subsection>>|<pageref|auto-10>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Section>|<with|font-family|<quote|ss>|Subsubsection>>|<pageref|auto-12>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Mathematics>|<with|font-family|<quote|ss>|Inline
      formula>>|<pageref|auto-14>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Mathematics>|<with|font-family|<quote|ss>|Displayed
      formula>>|<pageref|auto-15>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Focus>|<with|font-family|<quote|ss>|Numbered>>|<pageref|auto-16>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Mathematics>|<with|font-family|<quote|ss>|Several
      equations>>|<pageref|auto-17>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Focus>|<with|font-family|<quote|ss>|Numbered>>|<pageref|auto-18>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Image>|<with|font-family|<quote|ss>|Big
      figure>>|<pageref|auto-19>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Image>|<with|font-family|<quote|ss>|Insert
      image>>|<pageref|auto-20>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Table>|<with|font-family|<quote|ss>|Big
      table>>|<pageref|auto-22>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Table>|<with|font-family|<quote|ss>|Plain
      block>>|<pageref|auto-23>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Link>|<with|font-family|<quote|ss>|Label>>|<pageref|auto-25>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Link>|<with|font-family|<quote|ss>|Reference>>|<pageref|auto-26>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Link>|<with|font-family|<quote|ss>|Citation>|<with|font-family|<quote|ss>|Visible>>|<pageref|auto-27>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Link>|<with|font-family|<quote|ss>|Citation>|<with|font-family|<quote|ss>|Invisible>>|<pageref|auto-28>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Automatic>|<with|font-family|<quote|ss>|Bibliography>>|<pageref|auto-29>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Document>|<with|font-family|<quote|ss>|Update>|<with|font-family|<quote|ss>|Bibliography>>|<pageref|auto-30>>
    </associate>
    <\associate|table>
      <tuple|normal|<\surround|<hidden-binding|<tuple>|1>|>
        Image file formats currently recognized by
        T<rsub|<space|-0.4spc><move|<resize|<with|math-level|<quote|0>|E>||||0.5fn>|0fn|-0.1fn>><space|-0.4spc>X<rsub|<space|-0.4spc><move|<resize|M<space|-0.2spc>A<space|-0.4spc>CS||||0.5fn>|0fn|-0.1fn>>.
      </surround>|<pageref|auto-24>>
    </associate>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|1<space|2spc>Section>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-9><vspace|0.5fn>

      <with|par-left|<quote|1tab>|1.1<space|2spc>Subsection
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-11>>

      <with|par-left|<quote|2tab>|1.1.1<space|2spc>Subsubsection
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-13>>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|Bibliography>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-31><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>