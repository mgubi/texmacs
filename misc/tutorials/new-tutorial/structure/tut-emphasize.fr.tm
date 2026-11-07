<TeXmacs|2.1.4>

<style|<tuple|tmdoc|french>>

<\body>
  <tmdoc-title|Mettre du texte en relief>

  Supposons que vous vouliez écrire la phrase

  <\quote-env>
    A <em|gnu> is a hairy animal, which is not a clone of a unix.
  </quote-env>

  dans laquelle le mot \Pgnu\Q est mis en relief. Plutôt que de passer à une
  police italique, vous indiquez à <TeXmacs> que le mot est <em|mis en
  relief>, en le plaçant dans la balise <markup|em>. Il y a plusieurs façons
  de le faire.

  <paragraph*|Avec les menus>

  Tapez \PA \Q et choisissez <menu|Insert|Content tag|Emphasize>. Une zone
  mise en relief vide apparaît, entourée d'un cadre cyan, et la barre d'état
  indique que le curseur se trouve dans <verbatim|em> :

  <\big-envbox>
    A <em|<small-focus|<text-cursor>>>
  </big-envbox>

  Tapez le mot \Pgnu\Q : il apparaît en italique, puisqu'il se trouve dans
  la zone mise en relief.

  <\big-envbox>
    A <em|<small-focus|gnu<text-cursor>>>
  </big-envbox>

  Pour continuer avec du texte ordinaire, appuyez sur <key*|right> : le
  curseur quitte la zone mise en relief (le cadre cyan disparaît) sans se
  déplacer à l'écran. Tapez maintenant le reste de la phrase.

  <paragraph*|Avec une commande hybride>

  Au lieu d'utiliser le menu, vous pouvez aussi taper le nom de la balise
  après la touche <key|\\>. Tapez <key|\\ e m> : la barre d'état indique
  maintenant qu'un appui sur <key|return> mettra le texte en relief.
  Appuyez sur <key|return>, et vous vous retrouvez dans la même situation
  que ci-dessus. Cela fonctionne pour toutes les balises, pourvu que vous
  connaissiez leur nom, ainsi que pour les commandes <LaTeX> qui ont un
  équivalent dans <TeXmacs> ; par exemple, <key|\\ s e c t i o n> crée un
  titre de section.

  <paragraph*|Avec la sélection>

  Enfin, si le texte a déjà été tapé, sélectionnez le mot \Pgnu\Q avec la
  souris puis choisissez <menu|Insert|Content tag|Emphasize> : le texte
  sélectionné est placé dans la balise.

  <paragraph*|Pourquoi ne pas utiliser simplement l'italique ?>

  Le texte mis en relief ressemble généralement à du texte en italique, mais
  c'est le style qui en décide. Par exemple, dans un théorème, qui est déjà
  en italique, le texte mis en relief est composé en romain, afin de rester
  visible :

  <\quote-env>
    <\theorem>
      A <em|gnu> is a hairy animal.
    </theorem>
  </quote-env>

  De la même manière, la balise <markup|strong>
  (<menu|Insert|Content tag|Strong>) marque le texte <strong|important>, et
  la balise <markup|name> (<menu|Insert|Content tag|Name>) marque les noms
  de personnes comme <name|Joris>. Si vous décidez plus tard que le texte
  important doit être affiché en rouge plutôt qu'en gras, il vous suffit de
  changer le style, et non les nombreux endroits où <markup|strong> a été
  utilisée.

  L'exemple de cette section se trouve dans <verbatim|examples/hairy.tm>.

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
