<TeXmacs|2.1.4>

<style|<tuple|tmdoc|french>>

<\body>
  <tmdoc-title|Théorèmes, démonstrations et listes>

  Les textes mathématiques se composent de définitions, de théorèmes, de
  démonstrations, de remarques, etc. Dans <TeXmacs>, chacun d'eux est un
  <em|environnement>, que l'on trouve dans <menu|Insert|Enunciation> :
  <TeXmacs> les numérote et les affiche selon le style du document. Tapons
  le texte suivant, qui est aussi disponible dans
  <verbatim|examples/ramsey.tm> :

  <\big-envbox>
    <\theorem>
      Among six people, there are always three people who mutually know
      each other or three people who mutually do not know each other.
    </theorem>

    <\proof>
      Let <math|P> be an arbitrary person among the six. Then either one of
      the following cases holds:

      <\description>
        <item*|Case 1><math|P> knows at least three other persons, which
        may be called <math|Q>, <math|R> and <math|S>.

        <item*|Case 2><text-cursor>
      </description>
    </proof>
  </big-envbox>

  <paragraph*|Créer les environnements>

  Choisissez <menu|Insert|Enunciation|Theorem> et tapez l'énoncé du
  théorème ; le mot \PThéorème\Q et son numéro sont ajoutés par <TeXmacs>.
  Sortez ensuite du théorème en déplaçant le curseur au-delà de sa fin avec
  <key*|right>, ou avec <shortcut|(structured-exit-right)>, et choisissez
  <menu|Insert|Enunciation|Proof>. Dans la démonstration, une liste
  descriptive se commence avec <menu|Insert|Description|Default> : chaque
  fois que vous appuyez sur <key|return>, un nouvel élément est créé, dont
  vous tapez l'étiquette avant son texte.

  <paragraph*|Le contexte du curseur>

  À la position du curseur dans l'exemple ci-dessus, la partie droite de la
  barre d'état affiche <verbatim|proof description item*> : le curseur se
  trouve dans un élément d'une description, à l'intérieur d'une
  démonstration. Ce <em|contexte> détermine aussi le contenu du menu
  <menu|Focus> et de la barre d'outils de focus, qui proposent les actions
  pertinentes pour l'environnement le plus intérieur.

  <paragraph*|Changer d'environnement>

  Puisque <TeXmacs> sait que l'énoncé est un théorème, il est facile de
  changer d'avis. Placez le curseur dans le théorème et appuyez sur
  <shortcut|(variant-circulate (focus-tree) #t)>, ou utilisez la barre
  d'outils de focus : le théorème devient une proposition, un lemme, un
  corollaire... tandis que son texte reste le même. De la même manière,
  <shortcut|(numbered-toggle (focus-tree))> supprime ou rétablit le numéro
  du théorème.

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
