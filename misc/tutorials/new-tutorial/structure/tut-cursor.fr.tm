<TeXmacs|2.1.4>

<style|<tuple|tmdoc|french>>

<\body>
  <tmdoc-title|Le curseur et la structure>

  Considérez la phrase suivante, dans laquelle les mots \Pbold blue\Q ont
  été mis en gras (avec <shortcut|(toggle-bold)>) puis en bleu (avec
  <menu|Format|Color>) :

  <\quote-env>
    Some <with|font-series|bold|<with|color|blue|bold blue>> text.
  </quote-env>

  Les mots \Pbold blue\Q se trouvent dans deux éléments de balisage : l'un
  pour la police grasse, l'autre pour la couleur bleue. Où va une lettre
  lorsque vous la tapez à côté de ces mots ? <TeXmacs> répond à cette
  question en donnant au curseur plusieurs positions à la frontière du
  balisage, qui se ressemblent à l'écran mais qui sont différentes dans le
  document.

  Placez le curseur après \PSome \Q, et appuyez plusieurs fois sur
  <key*|right> en observant la barre d'état et les cadres autour du
  curseur :

  <\big-table|<descriptive-table|<tformat|<cwith|1|-1|1|-1|cell-halign|l>|<table|<row|<cell|Curseur>|<cell|Position>>|<row|<cell|Some
  <text-cursor><with|font-series|bold|<with|color|blue|bold blue>>
  text.>|<cell|en dehors du balisage>>|<row|<cell|Some
  <with|font-series|bold|<small-focus|<text-cursor><with|color|blue|bold
  blue>>> text.>|<cell|dans la police grasse>>|<row|<cell|Some
  <with|font-series|bold|<small-envbox|<small-focus|<with|color|blue|<text-cursor>bold
  blue>>>> text.>|<cell|dans la police grasse et la couleur
  bleue>>|<row|<cell|Some <with|font-series|bold|<with|color|blue|b<text-cursor>old
  blue>> text.>|<cell|après la lettre <verbatim|b>>>>>>>
    Déplacement du curseur dans \Pbold blue\Q.
  </big-table>

  Les premiers appuis sur <key*|right> ne déplacent pas le curseur à
  l'écran, mais le font entrer dans le balisage, un niveau à la fois.
  Lorsque vous tapez une lettre à l'une de ces positions, elle prend la
  police et la couleur de cette position : dans la première ligne, une
  lettre noire ordinaire, dans la deuxième, une lettre noire en gras, et
  dans la troisième, une lettre bleue en gras. Il en va de même à la fin de
  \Pbold blue\Q.

  Comme nous l'avons vu, l'élément de balisage le plus intérieur autour du
  curseur est entouré d'un cadre cyan, et les éléments extérieurs de cadres
  gris clair ; la partie droite de la barre d'état les énumère tous. Avec
  un peu de pratique, ces cadres vous permettent de voir d'un coup d'÷il où
  ira votre prochaine lettre.

  <tmdoc-copyright|1998\U2026|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
