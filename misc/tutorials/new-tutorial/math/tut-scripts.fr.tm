<TeXmacs|2.1.4>

<style|<tuple|tmdoc|french>>

<\body>
  <tmdoc-title|Indices, exposants et fractions>

  De nombreuses constructions mathématiques, comme les indices, les
  exposants, les fractions et les racines, comportent une ou plusieurs
  parties que vous devez remplir. <TeXmacs> insère la construction avec des
  parties vides et place le curseur dans la première ; vous tapez alors son
  contenu et vous en sortez lorsque vous avez terminé.

  <paragraph*|Indices et exposants>

  Pour taper <math|x<rsup|2>>, tapez <key|x>, puis <key|^> pour un exposant,
  puis <key|2>. Le curseur se trouve maintenant dans l'exposant, comme
  l'indique le cadre cyan :

  <\big-envbox>
    <math|x<rsup|<small-focus|2<math-cursor>>>>
  </big-envbox>

  Tout ce que vous tapez maintenant va dans l'exposant. Appuyez sur
  <key*|right> pour faire sortir le curseur de l'exposant, et continuez la
  formule. Les indices s'obtiennent de la même manière avec <key|_>, comme
  dans <math|x<rsub|n>>.

  <paragraph*|Fractions>

  Pour prolonger notre formule en <math|x<rsup|2>+<frac|a|b>>, tapez
  <key|+> puis <key|math f> (ou choisissez <menu|Insert|Fraction>). Une
  fraction vide apparaît, avec le curseur dans le numérateur :

  <\big-envbox>
    <math|x<rsup|2>+<frac|<small-focus|a<math-cursor>>|>>
  </big-envbox>

  Tapez <key|a>, appuyez sur <key*|down> pour passer au dénominateur, et
  tapez <key|b> :

  <\big-envbox>
    <math|x<rsup|2>+<frac|a|<small-focus|b<math-cursor>>>>
  </big-envbox>

  Enfin, appuyez sur <key*|right> pour sortir de la fraction. De la même
  manière, <key|math s> insère une racine carrée, comme dans
  <math|<sqrt|x+1>>, et <key|math s var> une racine <math|n>-ième, comme
  dans <math|<sqrt|x+1|3>>.

  <paragraph*|Supprimer une construction>

  Si vous avez inséré une construction par erreur, <key*|backspace> au début
  d'une partie vide la supprime. Plus généralement, <TeXmacs> garde trace de
  la structure de votre formule : le cadre cyan vous montre toujours dans
  quelle partie de quelle construction se trouve le curseur, et la barre
  d'état énumère toutes les constructions autour du curseur, comme
  <verbatim|math> et <verbatim|denominator>.

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
