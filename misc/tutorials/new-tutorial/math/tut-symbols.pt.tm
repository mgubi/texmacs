<TeXmacs|2.1.4>

<style|<tuple|tmdoc|portuguese>>

<\body>
  <tmdoc-title|Letras gregas e símbolos matemáticos>

  Dentro de uma fórmula, você obtém a maioria dos símbolos matemáticos com
  sequências curtas de teclas. Enquanto você digita uma dessas sequências, a
  barra de status mostra as teclas digitadas até então e o símbolo que elas
  produzem.

  <paragraph*|Letras gregas>

  Uma letra grega é obtida digitando a letra latina correspondente seguida
  de <key|var>. Por exemplo, para digitar a fórmula
  <math|\<lambda\>*ln x>, proceda da seguinte maneira:

  <\big-table|<descriptive-table|<tformat|<cwith|1|-1|1|-1|cell-halign|l>|<table|<row|<cell|Teclas>|<cell|Fórmula>|<cell|Observação>>|<row|<cell|<key|l>>|<cell|<math|l<math-cursor>>>|<cell|uma
  letra comum <math|l>>>|<row|<cell|<key|var>>|<cell|<math|\<lambda\><math-cursor>>>|<cell|o
  <math|l> se torna um <math|\<lambda\>>>>|<row|<cell|<key|*>>|<cell|<math|\<lambda\>*<math-cursor>>>|<cell|uma
  multiplicação invisível>>|<row|<cell|<key|l n>>|<cell|<math|\<lambda\>*ln<math-cursor>>>|<cell|o
  nome do logaritmo>>|<row|<cell|<key|space>>|<cell|<math|\<lambda\>*ln
  <math-cursor>>>|<cell|uma aplicação de função>>|<row|<cell|<key|x>>|<cell|<math|\<lambda\>*ln
  x<math-cursor>>>|<cell|o argumento>>>>>>
    Digitando a fórmula <math|\<lambda\>*ln x>.
  </big-table>

  Observe que a multiplicação digitada com <key|*> e a aplicação de função
  digitada com <key|space> são invisíveis, mas dizem ao <TeXmacs> o que a
  sua fórmula significa. Pressionar <key|var> várias vezes fornece as
  variantes da letra: <key|e var> produz <math|\<varepsilon\>>,
  <key|e var var> produz a constante <math|\<mathe\>> e <key|e var var
  var> produz <math|\<epsilon\>>. As letras gregas maiúsculas são
  obtidas da mesma maneira: <key|G var> produz <math|\<Gamma\>>.

  <paragraph*|Setas e outros símbolos>

  A maioria dos símbolos é obtida \Pdesenhando-os\Q com o teclado. Por
  exemplo, <key|- \<gtr\>> produz <math|\<rightarrow\>>, <key|= \<gtr\>>
  produz <math|\<Rightarrow\>>, <key|\<less\> => produz
  <math|\<leqslant\>> e <key|= /> produz <math|\<neq\>>. Em geral,
  <key|/> depois de um símbolo o <em|nega>, e <key|@> antes de um símbolo o
  coloca dentro de um círculo: <key|@ +> produz <math|\<oplus\>> e <key|@ x>
  produz <math|\<otimes\>>.

  <paragraph*|Variantes>

  Assim como para as letras gregas, a tecla <key|var> percorre as variantes
  de um símbolo. Por exemplo, partindo de <key|\<less\>> e pressionando
  <key|var> várias vezes, você obtém sucessivamente:

  <\big-table|<descriptive-table|<tformat|<cwith|1|-1|1|-1|cell-halign|l>|<table|<row|<cell|Teclas>|<cell|Símbolo>>|<row|<cell|<key|\<less\>>>|<cell|<math|\<less\>>>>|<row|<cell|<key|\<less\>
  var>>|<cell|<math|\<in\>>>>|<row|<cell|<key|\<less\> var
  var>>|<cell|<math|\<subset\>>>>|<row|<cell|<key|\<less\> var var
  var>>|<cell|<math|\<prec\>>>>|<row|<cell|<key|\<less\> var var var
  =>>|<cell|<math|\<preccurlyeq\>>>>|<row|<cell|<key|\<less\> var var var =
  var>>|<cell|<math|\<preceq\>>>>|<row|<cell|<key|\<less\> var var var = var
  />>|<cell|<math|\<npreceq\>>>>>>>>
    Variantes do símbolo <math|\<less\>>.
  </big-table>

  Se você passou do ponto, <key|S-var> volta à variante anterior.

  <paragraph*|Símbolos pelo nome>

  Se você conhece o nome <LaTeX> de um símbolo, também pode digitá-lo depois
  da tecla <key|\\>. Por exemplo, digitar <key|\\ h e a r t s u i t> e
  depois <key|return> produz <math|\<heartsuit\>>. Enquanto você digita o
  nome, a barra de status indica qual símbolo será inserido quando você
  pressionar <key|return>.

  <paragraph*|Menus de símbolos>

  Por fim, todos os símbolos também estão disponíveis nos menus de ícones da
  barra de ferramentas do modo e em <menu|Insert|Symbol>. Quando você deixa
  o ponteiro do mouse sobre um símbolo nesses menus, uma dica mostra o seu
  atalho de teclado, para que você possa aprender os atalhos dos símbolos
  que usa com frequência.

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
