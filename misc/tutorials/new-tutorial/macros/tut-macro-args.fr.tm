<TeXmacs|2.1.4>

<style|<tuple|tmdoc|french>>

<\body>
  <tmdoc-title|Macros avec arguments et préambule>

  <paragraph*|Le préambule>

  Les macros que vous définissez sont stockées dans le <em|préambule> de
  votre document, une partie cachée située à son début. Vous pouvez le
  consulter avec <menu|Tools|Macros|Edit preamble>, qui n'affiche que le
  préambule, et l'affiche sous forme de <em|code source> : au lieu de leur
  résultat, vous voyez les définitions elles-mêmes. Après la définition de
  la macro <markup|hi> de la section précédente, le préambule contient

  <\tm-fragment>
    <inactive*|<assign|hi|<macro|Hello world!>>>
  </tm-fragment>

  Ce qui se lit : <em|assigner> au nom <verbatim|hi> la <em|macro> dont le
  corps est <verbatim|Hello world!>. Choisissez à nouveau
  <menu|Tools|Macros|Edit preamble> pour revenir à votre texte.

  <paragraph*|Une macro avec un argument>

  Définissons maintenant une macro <markup|love>, telle que
  <inactive*|<love|free software>> produise le texte \PI love free software
  <with|color|red|<math|\<heartsuit\>>>\Q. Le texte qui suit \PI love\Q
  varie : il devient donc un <em|argument> de la macro, que nous appelons
  <src-arg|name>. La définition s'écrit

  <\tm-fragment>
    <inactive*|<assign|love|<macro|name|I love <arg|name>
    <with|color|red|<math|\<heartsuit\>>>>>>
  </tm-fragment>

  Dans l'éditeur de macro (<menu|Tools|Macros|New macro>), vous saisiriez
  le nom <verbatim|love> de la macro, puis vous lui donneriez un argument et
  vous taperiez le corps. Vous pouvez aussi taper la définition directement
  dans le préambule, de la même façon que le code source de n'importe quel
  document :

  <\enumerate>
    <item>Choisissez <menu|Tools|Macros|Edit preamble> et placez le curseur
    à la fin du préambule.

    <item>Tapez <key|\\ a s s i g n> et <key|return> : une balise
    <markup|assign> vide apparaît. Tapez le nom <verbatim|love> dans son
    premier champ, et passez au second champ avec <key*|right>.

    <item>Tapez <key|\\ m a c r o> et <key|return>, puis le nom de
    l'argument, <verbatim|name>. Appuyez sur <key|tab> : un nouveau champ
    est inséré après celui-ci, pour le corps de la macro.

    <item>Dans le corps, tapez \PI love \Q, puis le nom de l'argument après
    la touche <key|\\>, <key|\\ n a m e> : la barre d'état indique que
    <key|return> insérera l'argument <verbatim|name>. Appuyez sur
    <key|return> : la valeur de l'argument apparaîtra à cet endroit. Tapez
    une espace et ajoutez enfin le c÷ur : choisissez la couleur rouge dans
    <menu|Format|Color>, appuyez sur <key|$> et tapez
    <key|\\ h e a r t s u i t> et <key|return>.

    <item>Quittez le préambule avec <menu|Tools|Macros|Edit preamble>.
  </enumerate>

  Lorsque vous tapez maintenant <key|\\ l o v e> et <key|return> dans votre
  texte, une boîte avec un champ apparaît, dans laquelle vous tapez
  l'argument :

  <\big-envbox>
    I love <small-focus|free software<text-cursor>>
    <with|color|red|<math|\<heartsuit\>>>
  </big-envbox>

  Le document <verbatim|examples/love.tm> contient les macros de ce
  chapitre et quelques exemples de leur utilisation.

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
