<TeXmacs|2.1.4>

<style|<tuple|tmdoc|portuguese>>

<\body>
  <tmdoc-title|Macros com argumentos e o preâmbulo>

  <paragraph*|O preâmbulo>

  As macros que você define são armazenadas no <em|preâmbulo> do seu
  documento, uma parte oculta no seu início. Você pode examiná-lo com
  <menu|Tools|Macros|Edit preamble>, que mostra apenas o preâmbulo, e o
  mostra como <em|código-fonte>: em vez do seu resultado, você vê as
  próprias definições. Depois de definir a macro <markup|hi> da seção
  anterior, o preâmbulo contém

  <\tm-fragment>
    <inactive*|<assign|hi|<macro|Hello world!>>>
  </tm-fragment>

  Isso se lê: <em|atribua> ao nome <verbatim|hi> a <em|macro> cujo corpo é
  <verbatim|Hello world!>. Selecione <menu|Tools|Macros|Edit preamble>
  novamente para voltar ao seu texto.

  <paragraph*|Uma macro com argumento>

  Vamos agora definir uma macro <markup|love>, tal que
  <inactive*|<love|free software>> produza o texto \PI love free software
  <with|color|red|<math|\<heartsuit\>>>\Q. O texto depois de \PI love\Q
  varia, por isso ele se torna um <em|argumento> da macro, que chamamos de
  <src-arg|name>. A definição é

  <\tm-fragment>
    <inactive*|<assign|love|<macro|name|I love <arg|name>
    <with|color|red|<math|\<heartsuit\>>>>>>
  </tm-fragment>

  No editor de macros (<menu|Tools|Macros|New macro>), você digitaria o nome
  <verbatim|love> da macro, depois lhe daria um argumento e digitaria o
  corpo. Você também pode digitar a definição diretamente no preâmbulo, da
  mesma maneira que o código-fonte de qualquer documento:

  <\enumerate>
    <item>Escolha <menu|Tools|Macros|Edit preamble> e coloque o cursor no
    final do preâmbulo.

    <item>Digite <key|\\ a s s i g n> e <key|return>: aparece uma etiqueta
    <markup|assign> vazia. Digite o nome <verbatim|love> no seu primeiro
    campo e passe para o segundo campo com <key*|right>.

    <item>Digite <key|\\ m a c r o> e <key|return>, e depois o nome do
    argumento, <verbatim|name>. Pressione <key|tab>: um novo campo é
    inserido depois dele, para o corpo da macro.

    <item>No corpo, digite \PI love \Q e depois o nome do argumento depois
    da tecla <key|\\>, <key|\\ n a m e>: a barra de status informa que
    <key|return> inserirá o argumento <verbatim|name>. Pressione
    <key|return>: o valor do argumento aparecerá neste lugar. Digite um espaço
    e, por fim, acrescente o coração: selecione a cor vermelha em
    <menu|Format|Color>, pressione <key|$> e digite <key|\\ h e a r t s u i t>
    e <key|return>.

    <item>Saia do preâmbulo com <menu|Tools|Macros|Edit preamble>.
  </enumerate>

  Quando você agora digita <key|\\ l o v e> e <key|return> no seu texto,
  aparece uma caixa com um campo, no qual você digita o argumento:

  <\big-envbox>
    I love <small-focus|free software<text-cursor>>
    <with|color|red|<math|\<heartsuit\>>>
  </big-envbox>

  O documento <verbatim|examples/love.tm> contém as macros deste capítulo e
  algumas aplicações delas.

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
