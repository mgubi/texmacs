<TeXmacs|2.1.5>

<style|<tuple|tmdoc|asymptote|english>>

<\body>
  <tmdoc-title|<name|Asymptote> in a web browser>

  In the version of <TeXmacs> which runs in a web browser, the
  <name|Asymptote> plug-in needs nothing installed: <name|Asymptote> 3.15
  itself runs in the page, compiled to <name|WebAssembly> by
  <name|Asymptote-web> 0.3.3:

  <\itemize>
    <item>its sources are at
    <hlink|github.com/Julieisbaka/Asymptote-web|https://github.com/Julieisbaka/Asymptote-web>
    (its npm package <verbatim|asymptote-web>), and those of
    <name|Asymptote> at
    <hlink|asymptote.sourceforge.io|https://asymptote.sourceforge.io>;

    <item>both are free software, under the GNU Lesser General Public
    License, version<nbsp>3.
  </itemize>

  <name|Asymptote> runs apart from the page, so that <TeXmacs> stays
  responsive while a picture is made. The first picture takes a few seconds,
  the time to load it (about 7<nbsp>MB, kept by the browser afterwards); the
  next ones take a fraction of a second.

  <paragraph|The labels>

  <name|Asymptote-web> has no <TeX>: the labels of a picture are set by
  <TeXmacs>, from their <LaTeX>, in the fonts of the document, over the
  drawing of <name|Asymptote>, and aligned on their points as
  <name|Asymptote> aligns them. They can be selected, searched and edited as
  any other text.

  <paragraph|Sessions and executable folds>

  A session is started with <menu|Insert|Session|Asymptote>; an executable
  fold with <menu|Insert|Fold|Executable|Asymptote>. Type the code of a
  picture and press <shortcut|(kbd-return)> to make it (in a fold,
  <shortcut|(kbd-shift-return)> when its code has several lines);
  <shortcut|(kbd-return)> on the picture of a fold shows its code again.

  <paragraph|Examples>

  Each example below is an executable fold: put the cursor inside it and
  press <shortcut|(kbd-shift-return)> to make the picture, then
  <shortcut|(kbd-return)> on the picture to see its code again.

  A circle, an angle and a point:

  <\script-input|asymptote|default>
    size(5cm);

    draw(unitcircle);

    pair A = dir(40);

    draw((0,0)--A, blue);

    draw((0,0)--(1,0));

    draw(arc((0,0), 0.3, 0, 40));

    label("$\\theta$", 0.4*dir(20));

    dot("$A=(\\cos\\theta,\\sin\\theta)$", A, NE, blue);

    label("$O$", (0,0), SW);
  <|script-input>
    
  </script-input>

  The graph of a function, with its axes and their ticks:

  <\script-input|asymptote|default>
    import graph;

    size(7cm, 4cm, IgnoreAspect);

    real f(real x) {return sin(x);}

    draw(graph(f, 0, 2pi), red);

    xaxis("$x$", BottomTop, LeftTicks(Step=1));

    yaxis("$\\sin x$", LeftRight, RightTicks);
  <|script-input>
    
  </script-input>

  A right triangle:

  <\script-input|asymptote|default>
    size(5cm);

    pair A = (0,0), B = (4,0), C = (4,3);

    draw(A--B--C--cycle);

    draw(B+(-0.3,0)--B+(-0.3,0.3)--B+(0,0.3));

    label("$A$", A, SW); label("$B$", B, SE); label("$C$", C, NE);

    label("$a$", (A+B)/2, S); label("$b$", (B+C)/2, E);

    label("$c=\\sqrt{a^2+b^2}$", (A+C)/2, NW);
  <|script-input>
    
  </script-input>

  Labels made by a program (here a loop), and a label on a box:

  <\script-input|asymptote|default>
    size(5cm);

    fill(unitcircle, lightyellow);

    draw(unitcircle);

    for (int k = 0; k \<less\> 6; ++k) {

    \ \ pair z = dir(60*k);

    \ \ draw((0,0)--z, gray);

    \ \ dot(z, red);

    \ \ label("$\\omega^" + string(k) + "$", z, z);

    }

    label("$z^6 = 1$", (0,0), Fill(white));
  <|script-input>
    
  </script-input>

  A diagram, with text in bold and in colour:

  <\script-input|asymptote|default>
    size(6cm);

    draw(box((0,0), (2,1)));

    draw(box((3,0), (5,1)));

    draw((2,0.5)--(3,0.5), Arrow);

    label("input", (1,0.5));

    label("\\textbf{TeXmacs}", (4,0.5), blue);

    label("$f$", (2.5,0.5), N);
  <|script-input>
    
  </script-input>

  <paragraph|Editing the labels>

  The labels of a picture are <TeXmacs> text: click on one to edit it. In
  an executable fold, the code follows: when <shortcut|(kbd-return)> shows
  the code of a picture whose labels were edited, the string of each edited
  label (<verbatim|"$A$">...) is replaced in the code by the <LaTeX> of the
  label, provided the code has this string once. For instance, change
  <with|font-shape|italic|input> in the last example above into
  <with|font-shape|italic|data> and press <shortcut|(kbd-return)> twice. The
  labels made by a program (as the powers of <math|<omega>> above) are not
  put back in the code.

  <paragraph|Limits>

  <\itemize>
    <item>A rotated label is set upright.

    <item><name|Asymptote> leaves room for the labels from an estimate of
    their size, and chooses the ticks of the axes from its own font: give
    them a step when it chooses too few (<verbatim|LeftTicks(Step=1)>).

    <item>There are no pictures in three dimensions, and no other output
    than the picture in the document.

    <item>A first line <verbatim|// debug: svg> shows the picture as
    <name|Asymptote> made it, with its labels, as text.
  </itemize>

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
