<TeXmacs|2.1.4>

<style|<tuple|tmdoc|french>>

<\body>
  <tmdoc-title|Entrer dans le mode mathématique et en sortir>

  <TeXmacs> fait la distinction entre le texte ordinaire et les formules
  mathématiques. Dans les formules, les lettres sont composées en italique,
  l'espacement autour des symboles suit les conventions mathématiques et le
  clavier se comporte différemment, afin que l'on puisse taper rapidement
  les symboles. Pour taper une formule, vous devez donc d'abord entrer dans
  le <em|mode mathématique>.

  <paragraph*|Formules dans le texte>

  Supposons que vous ayez tapé \PHere comes a formula:\Q et que vous vouliez
  maintenant taper une petite formule sur la même ligne. Choisissez
  <menu|Insert|Mathematics|Inline formula>, ou appuyez simplement sur la
  touche <key|$> :

  <\big-envbox>
    Here comes a formula: <math|<small-focus|<math-cursor>>>
  </big-envbox>

  Plusieurs choses se sont produites : le curseur est devenu violet, une
  formule vide entourée d'un cadre cyan a été insérée, la partie gauche de
  la barre d'état indique désormais <verbatim|math> au lieu de
  <verbatim|text>, et la barre d'outils du mode affiche des icônes avec des
  symboles mathématiques. Tout ce que vous tapez maintenant va dans la
  formule, jusqu'à ce que vous en sortiez. Le moyen le plus simple de sortir
  de la formule est d'en faire sortir le curseur avec <key*|right> une fois
  que vous êtes à sa fin.

  <paragraph*|Formules centrées>

  Les formules plus grandes s'affichent mieux sur une ligne à part, comme

  <\equation*>
    a<rsup|2>+b<rsup|2>=c<rsup|2>.
  </equation*>

  Ces <em|formules centrées> s'insèrent avec
  <menu|Insert|Mathematics|Displayed formula> ou
  <shortcut|(make-equation*)>. Vous pouvez numéroter une formule centrée
  avec <shortcut|(numbered-toggle (focus-tree))>, et transformer une formule
  dans le texte en formule centrée (ou l'inverse) avec
  <shortcut|(variant-circulate (focus-tree) #t)>.

  Dans les sections suivantes, nous verrons comment taper le contenu des
  formules.

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
