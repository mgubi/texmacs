<TeXmacs|2.1.4>

<style|<tuple|tmdoc|portuguese>>

<\body>
  <tmdoc-title|Entendendo a janela do <TeXmacs>>

  A figura a seguir mostra as partes principais de uma janela do <TeXmacs>.
  Faremos referência a elas com frequência neste tutorial.

  <\big-figure>
    <tabular|<tformat|<twith|table-width|0.9par>|<twith|table-hmode|exact>|<cwith|1|-1|1|1|cell-hyphen|n>|<cwith|1|-1|1|1|cell-lborder|1ln>|<cwith|1|-1|1|1|cell-rborder|1ln>|<cwith|1|1|1|1|cell-tborder|1ln>|<cwith|-1|-1|1|1|cell-bborder|1ln>|<cwith|1|-1|1|1|cell-hpart|1>|<cwith|1|-1|2|2|cell-halign|l>|<cwith|1|-1|2|2|cell-lsep|1em>|<cwith|1|-1|1|-1|cell-valign|c>|<cwith|1|1|1|1|cell-background|pastel
    blue>|<cwith|1|1|1|1|cell-halign|c>|<cwith|2|5|1|1|cell-background|light
    grey>|<cwith|2|5|1|1|cell-bborder|0.5ln>|<cwith|6|6|1|1|cell-height|6em>|<cwith|6|6|1|1|cell-vmode|exact>|<cwith|6|6|1|1|cell-valign|t>|<cwith|6|6|1|1|cell-tsep|1em>|<cwith|6|6|1|1|cell-lsep|1.5em>|<cwith|7|7|1|1|cell-background|light
    grey>|<cwith|7|7|1|1|cell-tborder|0.5ln>|<table|<row|<cell|<with|font-family|ss|test.tm>>|<cell|<em|barra
    de título>>>|<row|<cell|<with|font-size|0.84|<menu-item|File><space|0.5em><menu-item|Edit><space|0.5em><menu-item|Insert><space|0.5em><menu-item|Focus><space|0.5em><menu-item|Format><space|0.5em><menu-item|Document><space|0.5em><menu-item|View><space|0.5em><menu-item|Go><space|0.5em><menu-item|Tools><space|0.5em><menu-item|Help>>>|<cell|<em|barra
    de menus>>>|<row|<cell|<icon|tm_new.png> <icon|tm_open.png>
    <icon|tm_save.png> <icon|tm_print.png> \ <icon|tm_copy.png>
    <icon|tm_undo.png>>|<cell|<em|barra de ferramentas principal>>>|<row|<cell|<with|font-family|ss|<em|ferramentas
    do modo atual (texto, matemática...)>>>|<cell|<em|barra de ferramentas
    do modo>>>|<row|<cell|<with|font-family|ss|<em|ferramentas da etiqueta
    no cursor>>>|<cell|<em|barra de ferramentas de foco>>>|<row|<cell|<text-cursor>Hello>|<cell|<em|área
    de edição> com o <em|cursor>>>|<row|<cell|<with|font-family|ss|text roman 10<htab|1em>start>>|<cell|<em|barra de status>>>>>>
  <|big-figure>
    As partes principais da janela do <TeXmacs>.
  </big-figure>

  A <em|barra de título> exibe o nome do seu documento. Enquanto você não
  tiver dado um nome ao documento, ele se chama <verbatim|No name [1]> (ou
  <verbatim|No name [2]> para o próximo documento novo, e assim por
  diante). Nas laterais da barra de título, as <em|decorações da janela>
  permitem minimizar, maximizar ou fechar a janela. Elas são desenhadas pelo
  seu sistema operacional, e não pelo <TeXmacs>, de modo que a sua aparência
  depende do seu sistema.

  O seu texto propriamente dito é exibido na <em|área de edição>, que mostra
  o documento tal como ele será impresso. Se o texto não couber na janela,
  você pode usar as <em|barras de rolagem>, a roda do mouse ou o touchpad
  para percorrê-lo. O <em|cursor> (também chamado de <em|caret>) indica a
  posição do texto em que você está digitando; ele é desenhado em vermelho
  no texto e em roxo nas fórmulas matemáticas. Você pode movê-lo com as
  teclas de seta ou clicando com o mouse.

  Os <em|menus> da <em|barra de menus> dão acesso a todos os recursos do
  <TeXmacs>. Por exemplo, o menu <menu|File> permite criar, carregar,
  salvar, exportar e imprimir documentos, o menu <menu|Insert> permite
  inserir novos elementos, como seções, listas, fórmulas, tabelas e imagens,
  o menu <menu|Format> permite mudar a fonte ou a cor do texto, e o menu
  <menu|Document> permite mudar as propriedades globais do documento, como o
  seu estilo, o seu idioma ou o tamanho da página. No <name|macOS>, a barra
  de menus fica no alto da tela, e não no alto da janela.

  Os <em|ícones> das <em|barras de ferramentas> dão acesso mais rápido às
  ações mais frequentes. A barra de ferramentas principal quase não muda; a
  <em|barra de ferramentas do modo> muda quando você passa do texto para a
  matemática, e a <em|barra de ferramentas de foco> muda conforme a
  marcação em que o cursor se encontra, como veremos no capítulo sobre texto
  estruturado. Muitos ícones são, na verdade, <em|menus de ícones>: um menu
  aparece quando você clica neles. Você pode ocultar ou mostrar as barras de
  ferramentas pelo menu <menu|View>.

  A <em|barra de status>, na parte inferior da janela, exibe informações
  relevantes sobre o seu documento. A sua parte esquerda indica se você
  está digitando texto ou matemática, e qual fonte e qual tamanho são usados
  na posição do cursor. A sua
  parte direita descreve a posição do cursor no documento: aqui, o cursor
  está no início (<verbatim|start>) de um parágrafo; mais adiante, ela
  mostrará qual caractere você acabou de digitar e em quais marcações o
  cursor se encontra. A barra de status também é usada para exibir
  mensagens, por exemplo quando um documento é salvo.

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
