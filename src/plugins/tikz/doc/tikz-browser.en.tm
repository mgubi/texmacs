<TeXmacs|2.1.5>

<style|<tuple|tmdoc|tikz|english>>

<\body>
  <tmdoc-title|<name|TikZ> in a web browser>

  In the version of <TeXmacs> which runs in a web browser, the <name|TikZ>
  plug-in does not need <LaTeX> nor <name|Python>: <name|TeX> itself runs in
  the browser. Pictures are made by <name|TikZJax> 1.6.0, a <name|TeX>
  compiled to <name|WebAssembly>, with <name|PGF>/<name|TikZ> and a few
  packages:

  <\itemize>
    <item>the version used is the one of
    <hlink|github.com/rod2ik/tikzjax|https://github.com/rod2ik/tikzjax> (its
    npm package <verbatim|@rod2ik/tikzjax>), which extends
    <hlink|github.com/kisonecat/tikzjax|https://github.com/kisonecat/tikzjax>
    by <name|Jim Fowler> and
    <hlink|github.com/drgrice1/tikzjax|https://github.com/drgrice1/tikzjax>
    by <name|Glenn Rice>;

    <item>it is free software, under the GNU General Public License,
    version<nbsp>3 or later.
  </itemize>

  <name|TikZJax> runs apart from the page, so that <TeXmacs> stays
  responsive while a picture is made. The first picture takes a few seconds,
  the time to load <name|TeX> and its files (about 5<nbsp>MB, kept by the
  browser afterwards); the next ones are faster.

  The lines of a picture are drawn as an image, but the text of its nodes is
  typeset by <TeXmacs> over it, in the fonts of the document: it can be
  selected, searched, and even edited (see below).

  <paragraph|Sessions and executable folds>

  A <name|TikZ> session is started with <menu|Insert|Session|TikZ>. It
  begins with a short reminder of what follows: the version of
  <name|TikZJax>, how to make a picture, and how to ask for packages. Type the
  commands of a picture (what goes inside
  <verbatim|\\begin{tikzpicture}><text-dots><verbatim|\\end{tikzpicture}>,
  which may be omitted) and press <shortcut|(kbd-return)> to make it; use
  <shortcut|(kbd-shift-return)> to start a new line of input.

  A picture inside the text of a document is best made with an
  <em|executable fold>, <menu|Insert|Fold|Executable|TikZ>: type its source
  in the fold and press <shortcut|(kbd-return)> to replace it by the picture.
  Pressing <shortcut|(kbd-return)> again on the picture shows its source
  again, to change it.

  <paragraph|Examples>

  Each example below is an executable fold: put the cursor inside it and
  press <shortcut|(kbd-shift-return)> (its source has several lines) to
  make the picture, then <shortcut|(kbd-return)> on the picture to see its
  source again.

  The graph of a function, with the labels of its axes:

  <\script-input|tikz|default>
    \\draw[-\<gtr\>] (-0.5,0) -- (4,0) node[right] {$x$};

    \\draw[-\<gtr\>] (0,-0.5) -- (0,3) node[above] {$y$};

    \\draw[thick,blue,domain=0:3.5] plot (\\x,{0.2*\\x*\\x}) node[right] {$f(x)=x^2/5$};

    \\filldraw[red] (2,0.8) circle (2pt) node[below right] {a point of the graph};
  <|script-input>
    
  </script-input>

  A right triangle, its vertices, sides and an angle:

  <\script-input|tikz|default>
    \\coordinate (A) at (0,0);

    \\coordinate (B) at (4,0);

    \\coordinate (C) at (4,3);

    \\draw[thick] (A) -- (B) -- (C) -- cycle;

    \\draw (3.7,0) -- (3.7,0.3) -- (4,0.3);

    \\draw (0.8,0) arc (0:36.87:0.8) node[midway,right] {$\\alpha$};

    \\node[below left] at (A) {$A$};

    \\node[below right] at (B) {$B$};

    \\node[above right] at (C) {$C$};

    \\node[below] at (2,0) {$a$};

    \\node[right] at (4,1.5) {$b$};

    \\node[above left] at (2,1.5) {$c=\\sqrt{a^2+b^2}$};
  <|script-input>
    
  </script-input>

  The unit circle and the cosine of an angle:

  <\script-input|tikz|default>
    \\draw[-\<gtr\>] (-1.5,0) -- (1.5,0) node[right] {$x$};

    \\draw[-\<gtr\>] (0,-1.5) -- (0,1.5) node[above] {$y$};

    \\draw[thick] (0,0) circle (1);

    \\draw[thick,blue] (0,0) -- (40:1) node[above right] {$P=(\\cos\\theta,\\sin\\theta)$};

    \\draw[red,thick] (40:1) -- (0.766,0) node[below] {$\\cos\\theta$};

    \\draw (0.3,0) arc (0:40:0.3);

    \\node at (20:0.5) {$\\theta$};
  <|script-input>
    
  </script-input>

  A finite automaton, its states and transitions:

  <\script-input|tikz|default>
    \\node[circle,draw,thick] (q0) at (0,0) {$q_0$};

    \\node[circle,draw,thick] (q1) at (3,0) {$q_1$};

    \\node[circle,draw,double,thick] (q2) at (6,0) {$q_2$};

    \\draw[-\<gtr\>,thick] (-1,0) -- (q0);

    \\draw[-\<gtr\>,thick] (q0) to[bend left] node[above] {$a$} (q1);

    \\draw[-\<gtr\>,thick] (q1) to[bend left] node[below] {$b$} (q0);

    \\draw[-\<gtr\>,thick] (q1) -- node[above] {$a$} (q2);

    \\draw[-\<gtr\>,thick] (q2) to[loop above] node[above] {$a,b$} (q2);
  <|script-input>
    
  </script-input>

  A tree, which draws the structure of an expression:

  <\script-input|tikz|default>
    \\node {$x+y\\cdot z$}

    \ \ child {node {$x$}}

    \ \ child {node {$y\\cdot z$}

    \ \ \ \ child {node {$y$}}

    \ \ \ \ child {node {$z$}}};
  <|script-input>
    
  </script-input>

  A commutative diagram, with the package <verbatim|tikz-cd>:

  <\script-input|tikz|default>
    % packages: tikz-cd

    \\begin{tikzcd}

    A \\arrow[r, "f"] \\arrow[d, "g"'] & B \\arrow[d, "h"] \\\\

    C \\arrow[r, "k"'] & D

    \\end{tikzcd}
  <|script-input>
    
  </script-input>

  An electric circuit, with the package <verbatim|circuitikz>:

  <\script-input|tikz|default>
    % packages: circuitikz

    \\draw (0,0) to[V, l=$V_0$] (0,3) to[R, l=$R$] (3,3) to[C, l=$C$] (3,0) -- (0,0);
  <|script-input>
    
  </script-input>

  <paragraph|Editing the text of a picture>

  The text of the nodes of a picture is <TeXmacs> text: click on it to edit
  it, as any other text of the document, formulas included. In an executable
  fold, the source of the picture follows: when <shortcut|(kbd-return)>
  shows the source of a picture whose labels were edited, the text of these
  nodes in the source is replaced by the <LaTeX> of the edited labels, so
  that the picture is made again with the new text. For instance, change
  <with|font-shape|italic|a point of the graph> in the first example above
  into <with|font-shape|italic|the minimum> and press
  <shortcut|(kbd-return)> twice.

  Only the text of the nodes which are written in the source can be edited
  this way: text made by a loop (<verbatim|\\foreach>), by the option
  <verbatim|label=>, or by a package (as the labels of the arrows of
  <verbatim|tikz-cd>) is part of the image, as are rotated nodes and nodes
  of several lines.

  <paragraph|Packages and libraries>

  The packages of <name|TikZJax> are <verbatim|pgfplots>,
  <verbatim|tikz-cd>, <verbatim|circuitikz>, <verbatim|chemfig>,
  <verbatim|tkz-tab>, <verbatim|yquant>, <verbatim|braids>,
  <verbatim|kinematikz>, <verbatim|tikz-feynhand>, <verbatim|physics>,
  <verbatim|pgf-spectra>, as well as <verbatim|amsmath>,
  <verbatim|amssymb>, <verbatim|mathtools>, <verbatim|bm>,
  <verbatim|cancel> and <verbatim|mhchem>. A picture asks for packages and <name|TikZ>
  libraries with lines at its beginning:

  <\verbatim-code>
    % packages: circuitikz, amsmath

    % libraries: arrows.meta, calc
  </verbatim-code>

  A source may also be a whole document, from
  <verbatim|\\documentclass> to <verbatim|\\end{document}>, with its
  own preamble. Other packages, and fonts other than those of <name|Computer
  Modern> and the <name|AMS>, are not available in the browser. When
  <name|TeX> fails, its error is shown in place of the picture.

  <tmdoc-copyright|2026|Massimiliano Gubinelli>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify
  this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software
  Foundation; with no Invariant Sections, with no Front-Cover Texts, and
  with no Back-Cover Texts. A copy of the license is included in the
  section entitled "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
  </collection>
</initial>
