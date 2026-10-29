<TeXmacs|2.1.4>

<style|<tuple|tmdoc|french>>

<\body>
  <tmdoc-title|Lettres grecques et symboles mathématiques>

  Dans une formule, vous obtenez la plupart des symboles mathématiques avec
  de courtes séquences de touches. Pendant que vous tapez une telle
  séquence, la barre d'état affiche les touches tapées jusque-là et le
  symbole qu'elles produisent.

  <paragraph*|Lettres grecques>

  Une lettre grecque s'obtient en tapant la lettre latine correspondante
  suivie de <key|var>. Par exemple, pour taper la formule
  <math|\<lambda\>*ln x>, procédez comme suit :

  <\big-table|<descriptive-table|<tformat|<cwith|1|-1|1|-1|cell-halign|l>|<table|<row|<cell|Touches>|<cell|Formule>|<cell|Remarque>>|<row|<cell|<key|l>>|<cell|<math|l<math-cursor>>>|<cell|une
  lettre ordinaire <math|l>>>|<row|<cell|<key|var>>|<cell|<math|\<lambda\><math-cursor>>>|<cell|le
  <math|l> devient un <math|\<lambda\>>>>|<row|<cell|<key|*>>|<cell|<math|\<lambda\>*<math-cursor>>>|<cell|une
  multiplication invisible>>|<row|<cell|<key|l n>>|<cell|<math|\<lambda\>*ln<math-cursor>>>|<cell|le
  nom du logarithme>>|<row|<cell|<key|space>>|<cell|<math|\<lambda\>*ln
  <math-cursor>>>|<cell|une application de fonction>>|<row|<cell|<key|x>>|<cell|<math|\<lambda\>*ln
  x<math-cursor>>>|<cell|l'argument>>>>>>
    Saisie de la formule <math|\<lambda\>*ln x>.
  </big-table>

  Notez que la multiplication tapée avec <key|*> et l'application de
  fonction tapée avec <key|space> sont invisibles, mais elles indiquent à
  <TeXmacs> ce que signifie votre formule. Appuyer plusieurs fois sur
  <key|var> donne les variantes de la lettre : <key|e var> donne
  <math|\<varepsilon\>>, <key|e var var> donne la constante
  <math|\<mathe\>> et <key|e var var var> donne <math|\<epsilon\>>. Les
  lettres grecques majuscules s'obtiennent de la même manière : <key|G var>
  donne <math|\<Gamma\>>.

  <paragraph*|Flèches et autres symboles>

  La plupart des symboles s'obtiennent en les \Pdessinant\Q au clavier. Par
  exemple, <key|- \<gtr\>> donne <math|\<rightarrow\>>, <key|= \<gtr\>>
  donne <math|\<Rightarrow\>>, <key|\<less\> => donne
  <math|\<leqslant\>> et <key|= /> donne <math|\<neq\>>. En général,
  <key|/> après un symbole en donne la <em|négation>, et <key|@> avant un
  symbole l'entoure d'un cercle : <key|@ +> donne <math|\<oplus\>> et
  <key|@ x> donne <math|\<otimes\>>.

  <paragraph*|Variantes>

  Comme pour les lettres grecques, la touche <key|var> parcourt les
  variantes d'un symbole. Par exemple, en partant de <key|\<less\>> et en
  appuyant plusieurs fois sur <key|var>, vous obtenez successivement :

  <\big-table|<descriptive-table|<tformat|<cwith|1|-1|1|-1|cell-halign|l>|<table|<row|<cell|Touches>|<cell|Symbole>>|<row|<cell|<key|\<less\>>>|<cell|<math|\<less\>>>>|<row|<cell|<key|\<less\>
  var>>|<cell|<math|\<in\>>>>|<row|<cell|<key|\<less\> var
  var>>|<cell|<math|\<subset\>>>>|<row|<cell|<key|\<less\> var var
  var>>|<cell|<math|\<prec\>>>>|<row|<cell|<key|\<less\> var var var
  =>>|<cell|<math|\<preccurlyeq\>>>>|<row|<cell|<key|\<less\> var var var =
  var>>|<cell|<math|\<preceq\>>>>|<row|<cell|<key|\<less\> var var var = var
  />>|<cell|<math|\<npreceq\>>>>>>>>
    Variantes du symbole <math|\<less\>>.
  </big-table>

  Si vous êtes allé trop loin, <key|S-var> revient à la variante
  précédente.

  <paragraph*|Symboles par leur nom>

  Si vous connaissez le nom <LaTeX> d'un symbole, vous pouvez aussi le taper
  après la touche <key|\\>. Par exemple, taper <key|\\ h e a r t s u i t>
  puis <key|return> donne <math|\<heartsuit\>>. Pendant que vous tapez le
  nom, la barre d'état vous indique quel symbole sera inséré lorsque vous
  appuierez sur <key|return>.

  <paragraph*|Menus de symboles>

  Enfin, tous les symboles sont aussi disponibles dans les menus d'icônes de
  la barre d'outils du mode, ainsi que dans <menu|Insert|Symbol>. Lorsque
  vous laissez le pointeur de la souris sur un symbole dans ces menus, une
  info-bulle affiche son raccourci clavier, ce qui vous permet d'apprendre
  les raccourcis des symboles que vous utilisez souvent.

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
