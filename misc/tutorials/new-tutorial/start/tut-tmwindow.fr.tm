<TeXmacs|2.1.4>

<style|<tuple|tmdoc|french>>

<\body>
  <tmdoc-title|Comprendre la fenêtre de <TeXmacs>>

  La figure suivante montre les principales parties d'une fenêtre de
  <TeXmacs>. Nous y ferons souvent référence dans ce tutoriel.

  <\big-figure>
    <tabular|<tformat|<twith|table-width|0.9par>|<twith|table-hmode|exact>|<cwith|1|-1|1|1|cell-hyphen|n>|<cwith|1|-1|1|1|cell-lborder|1ln>|<cwith|1|-1|1|1|cell-rborder|1ln>|<cwith|1|1|1|1|cell-tborder|1ln>|<cwith|-1|-1|1|1|cell-bborder|1ln>|<cwith|1|-1|1|1|cell-hpart|1>|<cwith|1|-1|2|2|cell-halign|l>|<cwith|1|-1|2|2|cell-lsep|1em>|<cwith|1|-1|1|-1|cell-valign|c>|<cwith|1|1|1|1|cell-background|pastel
    blue>|<cwith|1|1|1|1|cell-halign|c>|<cwith|2|5|1|1|cell-background|light
    grey>|<cwith|2|5|1|1|cell-bborder|0.5ln>|<cwith|6|6|1|1|cell-height|6em>|<cwith|6|6|1|1|cell-vmode|exact>|<cwith|6|6|1|1|cell-valign|t>|<cwith|6|6|1|1|cell-tsep|1em>|<cwith|6|6|1|1|cell-lsep|1.5em>|<cwith|7|7|1|1|cell-background|light
    grey>|<cwith|7|7|1|1|cell-tborder|0.5ln>|<table|<row|<cell|<with|font-family|ss|test.tm>>|<cell|<em|barre
    de titre>>>|<row|<cell|<with|font-size|0.84|<menu-item|File><space|0.5em><menu-item|Edit><space|0.5em><menu-item|Insert><space|0.5em><menu-item|Focus><space|0.5em><menu-item|Format><space|0.5em><menu-item|Document><space|0.5em><menu-item|View><space|0.5em><menu-item|Go><space|0.5em><menu-item|Tools><space|0.5em><menu-item|Help>>>|<cell|<em|barre
    de menus>>>|<row|<cell|<icon|tm_new.png> <icon|tm_open.png>
    <icon|tm_save.png> <icon|tm_print.png> \ <icon|tm_copy.png>
    <icon|tm_undo.png>>|<cell|<em|barre d'outils principale>>>|<row|<cell|<with|font-family|ss|<em|outils
    pour le mode courant (texte, mathématiques...)>>>|<cell|<em|barre
    d'outils du mode>>>|<row|<cell|<with|font-family|ss|<em|outils pour la
    balise au curseur>>>|<cell|<em|barre d'outils de focus>>>|<row|<cell|<text-cursor>Hello>|<cell|<em|zone
    d'édition> avec le <em|curseur>>>|<row|<cell|<with|font-family|ss|text roman 10<htab|1em>start>>|<cell|<em|barre d'état>>>>>>
  <|big-figure>
    Les principales parties de la fenêtre de <TeXmacs>.
  </big-figure>

  La <em|barre de titre> affiche le nom de votre document. Tant que vous
  n'avez pas donné de nom à votre document, celui-ci s'appelle
  <verbatim|No name [1]> (ou <verbatim|No name [2]> pour le nouveau document
  suivant, et ainsi de suite). Sur les côtés de la barre de titre, les
  <em|décorations de la fenêtre> vous permettent de réduire, d'agrandir ou
  de fermer la fenêtre. Elles sont dessinées par votre système
  d'exploitation et non par <TeXmacs>, si bien que leur apparence dépend de
  votre système.

  Votre texte lui-même est affiché dans la <em|zone d'édition>, qui montre
  votre document tel qu'il sera imprimé. Si le texte ne tient pas dans la
  fenêtre, vous pouvez utiliser les <em|barres de défilement>, la molette de
  votre souris ou votre pavé tactile pour le faire défiler. Le <em|curseur>
  (aussi appelé <em|point d'insertion>) indique la position dans le texte où
  vous tapez ; il est dessiné en rouge dans le texte et en violet dans les
  formules mathématiques. Vous pouvez le déplacer avec les touches fléchées
  ou en cliquant avec la souris.

  Les <em|menus> de la <em|barre de menus> donnent accès à toutes les
  fonctionnalités de <TeXmacs>. Par exemple, le menu <menu|File> permet de
  créer, charger, sauvegarder, exporter et imprimer des documents, le menu
  <menu|Insert> d'insérer de nouveaux éléments tels que des sections, des
  listes, des formules, des tableaux et des images, le menu <menu|Format> de
  changer la police ou la couleur de votre texte, et le menu
  <menu|Document> de modifier les propriétés globales de votre document,
  comme son style, sa langue ou le format de ses pages. Sous <name|macOS>, la
  barre de menus se trouve en haut de l'écran plutôt qu'en haut de la
  fenêtre.

  Les <em|icônes> des <em|barres d'outils> donnent un accès plus rapide aux
  actions les plus fréquentes. La barre d'outils principale ne change
  guère ; la <em|barre d'outils du mode> change lorsque vous passez du texte
  aux mathématiques, et la <em|barre d'outils de focus> change selon
  l'élément de balisage dans lequel se trouve le curseur, comme nous le
  verrons dans le chapitre sur le texte structuré. Beaucoup d'icônes sont en
  fait des <em|menus d'icônes> : un menu apparaît lorsque vous cliquez
  dessus. Vous pouvez masquer ou afficher les barres d'outils à l'aide du
  menu <menu|View>.

  La <em|barre d'état>, en bas de la fenêtre, affiche des informations
  utiles sur votre document. Sa partie gauche vous indique si vous tapez du
  texte ou des mathématiques, ainsi que la police et la taille utilisées à
  la position du curseur. Sa partie droite décrit la position du curseur
  dans le document : ici, le curseur est au début (<verbatim|start>) d'un
  paragraphe ; plus tard, elle vous montrera quel caractère vous venez de
  taper et dans quels éléments de balisage se trouve le curseur. La barre
  d'état sert aussi à afficher des messages, par exemple lorsqu'un document
  a été sauvegardé.

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
