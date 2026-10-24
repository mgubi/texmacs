<TeXmacs|2.1.4>

<style|<tuple|tmdoc|french>>

<\body>
  <tmdoc-title|Une première macro>

  Définissons une macro <markup|hi> qui produit le texte \PHello
  world!\Q.

  <paragraph*|Définir la macro>

  La façon la plus simple de définir une macro est de partir d'un exemple.
  Tapez le texte <verbatim|Hello world!> dans votre document, sélectionnez-le
  et choisissez <menu|Tools|Macros|New macro>. L'<em|éditeur de macro>
  apparaît : il montre le corps de la nouvelle macro, c'est-à-dire le texte
  sélectionné, sous un champ destiné au nom de la macro :

  <\big-envbox>
    <inline-tag|<with|color|red|font-shape|italic|enter-name>>
    <math|\<assign\>>

    Hello world!
  </big-envbox>

  Cliquez sur le champ <with|color|red|font-shape|italic|enter-name> et
  tapez le nom <verbatim|hi>. Vous pouvez encore modifier le corps, qui peut
  contenir toute sorte de contenu : texte mis en forme, formules, tableaux,
  images... Cliquez ensuite sur <menu|Ok>. La macro <markup|hi> est
  maintenant définie dans votre document.

  Si vous ouvrez l'éditeur de macro sans sélection, le nom et le corps sont
  tous deux vides, et vous les tapez de la même manière.

  <paragraph*|Utiliser la macro>

  Désormais, partout où vous voulez voir apparaître le texte \PHello
  world!\Q, tapez <key|\\ h i> et <key|return>. Pendant que vous tapez le
  nom, la barre d'état confirme qu'il existe une macro <markup|hi>. Vous
  pouvez aussi choisir <menu|Insert|Macro|hi> : le menu <menu|Insert|Macro>
  liste toutes les macros de votre document.

  Lorsque le curseur est à côté du texte produit par la macro, la barre
  d'état affiche <verbatim|hi> : <TeXmacs> se souvient que ce texte est une
  application de la macro, et non du texte ordinaire. En particulier, si
  vous modifiez plus tard la définition de <markup|hi>, par exemple en
  \PHello everybody!\Q, le texte change aussitôt partout dans votre
  document. Pour modifier la définition, placez le curseur à côté d'une
  application de la macro et choisissez
  <menu|Focus|Preferences|Edit macro>, ou utilisez
  <menu|Tools|Macros|Edit macros>.

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
