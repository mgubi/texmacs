<TeXmacs|2.1.4>

<style|<tuple|tmdoc|portuguese>>

<\body>
  <tmdoc-title|O cursor e a estrutura>

  Considere a frase a seguir, na qual as palavras \Pbold blue\Q foram
  digitadas em negrito (com <shortcut|(toggle-bold)>) e depois em azul (com
  <menu|Format|Color>):

  <\quote-env>
    Some <with|font-series|bold|<with|color|blue|bold blue>> text.
  </quote-env>

  As palavras \Pbold blue\Q estão dentro de duas marcações: uma para a
  fonte em negrito e outra para a cor azul. Para onde vai uma letra quando
  você a digita ao lado dessas palavras? O <TeXmacs> responde a essa
  pergunta dando ao cursor várias posições na fronteira da marcação, que
  parecem iguais na tela, mas são diferentes no documento.

  Coloque o cursor depois de \PSome \Q e pressione <key*|right> várias vezes
  enquanto observa a barra de status e as caixas em torno do cursor:

  <\big-table|<descriptive-table|<tformat|<cwith|1|-1|1|-1|cell-halign|l>|<table|<row|<cell|Cursor>|<cell|Posição>>|<row|<cell|Some
  <text-cursor><with|font-series|bold|<with|color|blue|bold blue>>
  text.>|<cell|fora da marcação>>|<row|<cell|Some
  <with|font-series|bold|<small-focus|<text-cursor><with|color|blue|bold
  blue>>> text.>|<cell|dentro da fonte em negrito>>|<row|<cell|Some
  <with|font-series|bold|<small-envbox|<small-focus|<with|color|blue|<text-cursor>bold
  blue>>>> text.>|<cell|dentro da fonte em negrito e da cor
  azul>>|<row|<cell|Some <with|font-series|bold|<with|color|blue|b<text-cursor>old
  blue>> text.>|<cell|depois da letra <verbatim|b>>>>>>>
    Movendo o cursor para dentro de \Pbold blue\Q.
  </big-table>

  Os primeiros toques em <key*|right> não movem o cursor na tela, mas o
  movem para dentro da marcação, um nível de cada vez. Quando você digita
  uma letra em uma dessas posições, ela recebe a fonte e a cor dessa
  posição: na primeira linha, uma letra preta comum; na segunda, uma letra
  preta em negrito; e na terceira, uma letra azul em negrito. O mesmo
  acontece no final de \Pbold blue\Q.

  Como vimos, a marcação mais interna em torno do cursor é cercada por uma
  caixa ciano, e as mais externas por caixas cinza-claro; a parte direita da
  barra de status lista todas elas. Com um pouco de prática, essas caixas
  permitem ver de relance para onde irá a sua próxima letra.

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
