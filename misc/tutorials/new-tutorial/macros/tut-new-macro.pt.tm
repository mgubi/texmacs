<TeXmacs|2.1.4>

<style|<tuple|tmdoc|portuguese>>

<\body>
  <tmdoc-title|Uma primeira macro>

  Vamos definir uma macro <markup|hi> que produz o texto \PHello
  world!\Q.

  <paragraph*|Definindo a macro>

  A maneira mais simples de definir uma macro é partir de um exemplo.
  Digite o texto <verbatim|Hello world!> no seu documento, selecione-o e
  escolha <menu|Tools|Macros|New macro>. Aparece o <em|editor de macros>:
  ele mostra o corpo da nova macro, que é o texto selecionado, abaixo de um
  campo para o nome da macro:

  <\big-envbox>
    <inline-tag|<with|color|red|font-shape|italic|enter-name>>
    <math|\<assign\>>

    Hello world!
  </big-envbox>

  Clique no campo <with|color|red|font-shape|italic|enter-name> e digite o
  nome <verbatim|hi>. Você ainda pode modificar o corpo, que pode conter
  qualquer tipo de conteúdo: texto formatado, fórmulas, tabelas, imagens...
  Depois clique em <menu|Ok>. A macro <markup|hi> agora está definida no seu
  documento.

  Se você abrir o editor de macros sem uma seleção, tanto o nome quanto o
  corpo estarão vazios, e você os digita da mesma maneira.

  <paragraph*|Usando a macro>

  Agora, onde quer que você queira que o texto \PHello world!\Q apareça,
  digite <key|\\ h i> e <key|return>. Enquanto você digita o nome, a barra
  de status confirma que existe uma macro <markup|hi>. Você também pode
  escolher <menu|Insert|Macro|hi>: o menu <menu|Insert|Macro> lista todas as
  macros do seu documento.

  Quando o cursor está ao lado do texto produzido pela macro, a barra de
  status mostra <verbatim|hi>: o <TeXmacs> lembra que esse texto é uma
  aplicação da macro, e não texto comum. Em particular, se mais tarde você
  mudar a definição de <markup|hi>, por exemplo para \PHello everybody!\Q,
  o texto muda imediatamente em todo o documento. Para mudar a definição,
  coloque o cursor ao lado de uma aplicação da macro e escolha
  <menu|Focus|Preferences|Edit macro>, ou use
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
