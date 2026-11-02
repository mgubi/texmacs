<TeXmacs|2.1.4>

<style|<tuple|tmdoc|portuguese>>

<\body>
  <tmdoc-title|Reutilizando as suas macros em outros documentos>

  As macros do preâmbulo só existem no seu documento. Se você quiser usá-las
  também em outros documentos, coloque-as em um <em|pacote de estilo>: um
  arquivo com o sufixo <verbatim|.ts> que contém definições de macros e que
  você pode acrescentar a qualquer documento.

  <paragraph*|Criando o pacote>

  Abra o documento com as suas macros e escolha
  <menu|Tools|Macros|Extract style package>. O <TeXmacs> abre um novo
  documento no estilo <tmstyle|source>, que contém um cabeçalho (com o nome
  de pacote <verbatim|love-macros>, que você pode mudar para
  <verbatim|love>), os parâmetros de estilo do seu documento, se houver, e
  todas as definições do preâmbulo:

  <\tm-fragment>
    <inactive*|<assign|hi|<macro|Hello world!>>>

    <inactive*|<assign|love|<macro|name|I love <arg|name>
    <with|color|red|<math|\<heartsuit\>>>>>>
  </tm-fragment>

  Salve esse documento na pasta <verbatim|packages> da sua pasta pessoal do
  <TeXmacs>, com o nome <verbatim|love.ts>. Essa pasta pessoal é
  <verbatim|~/.TeXmacs> no <name|Linux> e no <name|macOS> (isto é, a pasta
  <verbatim|.TeXmacs> na sua pasta de usuário), e
  <verbatim|%APPDATA%\\TeXmacs> no <name|Windows>; o <TeXmacs> cria a pasta
  <verbatim|packages> dentro dela ao ser iniciado. No <name|macOS>, o
  seletor de arquivos oculta as pastas cujo nome começa com um ponto:
  pressione <verbatim|Cmd-Shift-.> para mostrar <verbatim|.TeXmacs>. O
  pacote de exemplo deste tutorial é <verbatim|examples/love.ts>.

  <paragraph*|Usando o pacote>

  Em qualquer documento, você agora pode escolher
  <menu|Document|Style|Add package|Add other package> e digitar
  <verbatim|love>: as macros <markup|hi> e <markup|love> ficam disponíveis,
  exatamente como se tivessem sido definidas no preâmbulo do documento. Se
  você mudar o pacote mais tarde, todos os documentos que o usam levam a
  mudança em conta. Um pacote pode ser removido de um documento no menu
  <menu|Document|Style>.

  Você também pode criar um pacote de estilo do zero, escolhendo
  <menu|Document|Style|source> para um novo documento, digitando as
  definições e salvando-o com o sufixo <verbatim|.ts>. A escrita de arquivos
  de estilo é explicada em detalhes em
  <menu|Help|Manual|Writing your own style files>.

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
