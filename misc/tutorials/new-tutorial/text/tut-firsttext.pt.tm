<TeXmacs|2.1.4>

<style|<tuple|tmdoc|portuguese>>

<\body>
  <tmdoc-title|Escrevendo, salvando, imprimindo e carregando um texto simples>

  <paragraph*|Digitação>

  Depois de dar um nome ao seu documento, você pode começar a digitar.
  Basta digitar o texto como em uma máquina de escrever: o <TeXmacs> quebra
  as linhas automaticamente quando ficam longas demais, e também as
  justifica, isto é, ajusta os espaços entre as palavras para que todas as
  linhas tenham a mesma largura. Você só precisa pressionar <key|return>
  para começar um novo parágrafo. O texto a seguir é um exercício simples:

  <\big-figure>
    <\big-envbox>
      Hey you there!

      Would you be so kind to enroll the gnus and gnats around you to try
      <TeXmacs> once?<text-cursor>
    </big-envbox>
  <|big-figure>
    Um texto simples digitado com o <TeXmacs> (<verbatim|examples/test.tm>).
  </big-figure>

  Se você cometer um erro, <key*|backspace> apaga o caractere antes do
  cursor, e <menu|Edit|Undo> (<shortcut|(undo 0)>) desfaz as suas últimas
  alterações. Caso o seu texto contenha caracteres que não estão no seu
  teclado, como letras acentuadas, você pode digitá-los com os métodos do
  seu sistema; veremos mais adiante como digitar símbolos matemáticos.

  <paragraph*|Salvando>

  Quando terminar de digitar, salve o documento com <menu|File|Save>,
  com a entrada <menu|Save> do menu do ícone <icon|tm_save.png>, ou com <shortcut|(save-buffer)>. Uma
  mensagem na barra de status confirma que o documento foi salvo.

  Para a sua segurança, o <TeXmacs> também <em|salva automaticamente> o seu
  documento a cada dois minutos, em um arquivo de cópia de segurança
  separado. Se você esquecer de salvar o documento antes de fechar o
  <TeXmacs>, ou se o seu computador parar por algum motivo, o <TeXmacs>
  proporá recuperar as alterações não salvas na próxima vez que você abrir
  o documento.

  <paragraph*|Imprimindo e exportando>

  Normalmente você vai querer imprimir o documento, ou enviá-lo a pessoas
  que não usam o <TeXmacs>. Você imprime o documento com <menu|File|Print|Print buffer>,
  usando as impressoras do seu sistema. Também pode transformá-lo em um
  arquivo PDF, que pode ser lido em qualquer computador, com
  <menu|File|Export|Pdf>. O tamanho do papel (A4 na Europa e no Brasil,
  Letter nos EUA) e as margens são definidos em <menu|Document|Page>.

  <paragraph*|Carregando>

  Para recuperar o seu texto depois de fechar o <TeXmacs>, use
  <menu|File|Load>, a entrada <menu|Load> do menu do ícone
  <icon|tm_open.png>, ou o atalho
  <shortcut|(open-document)>. Aparece um seletor de arquivos, no qual você
  seleciona o arquivo <verbatim|test.tm> e confirma, ou simplesmente clica
  duas vezes nele. Os seus documentos recentes também estão listados em
  <menu|File|Recent>.

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
