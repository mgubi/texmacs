<TeXmacs|2.1.4>

<style|<tuple|tmdoc|portuguese>>

<\body>
  <tmdoc-title|Destacando texto>

  Suponha que você queira escrever a frase

  <\quote-env>
    A <em|gnu> is a hairy animal, which is not a clone of a unix.
  </quote-env>

  na qual a palavra \Pgnu\Q está destacada. Em vez de mudar para uma fonte
  itálica, você diz ao <TeXmacs> que a palavra está <em|destacada>,
  colocando-a dentro da etiqueta <markup|em>. Há várias maneiras de fazer
  isso.

  <paragraph*|Usando os menus>

  Digite \PA \Q e selecione <menu|Insert|Content tag|Emphasize>. Uma região
  destacada vazia aparece, cercada por uma caixa ciano, e a barra de status
  indica que o cursor está dentro de <verbatim|em>:

  <\big-envbox>
    A <em|<small-focus|<text-cursor>>>
  </big-envbox>

  Digite a palavra \Pgnu\Q: ela aparece em itálico, pois está dentro da
  região destacada.

  <\big-envbox>
    A <em|<small-focus|gnu<text-cursor>>>
  </big-envbox>

  Para continuar com texto comum, pressione <key*|right>: o cursor sai da
  região destacada (a caixa ciano desaparece) sem se mover na tela. Agora
  digite o resto da frase.

  <paragraph*|Usando um comando híbrido>

  Em vez de usar o menu, você também pode digitar o nome da etiqueta depois
  da tecla <key|\\>. Digite <key|\\ e m>: a barra de status agora informa
  que pressionar <key|return> destacará o texto. Pressione <key|return>, e
  você estará na mesma situação de antes. Isso funciona para qualquer
  etiqueta, desde que você saiba o seu nome, e também para os comandos do
  <LaTeX> que têm um equivalente no <TeXmacs>; por exemplo,
  <key|\\ s e c t i o n> cria um título de seção.

  <paragraph*|Usando a seleção>

  Por fim, se o texto já foi digitado, selecione a palavra \Pgnu\Q com o
  mouse e depois escolha <menu|Insert|Content tag|Emphasize>: o texto
  selecionado é colocado dentro da etiqueta.

  <paragraph*|Por que não usar simplesmente itálico?>

  O texto destacado costuma ter a aparência de texto em itálico, mas quem
  decide é o estilo. Por exemplo, dentro de um teorema, que já está em
  itálico, o texto destacado é composto em fonte reta, para que continue
  visível:

  <\quote-env>
    <\theorem>
      A <em|gnu> is a hairy animal.
    </theorem>
  </quote-env>

  Da mesma maneira, a etiqueta <markup|strong>
  (<menu|Insert|Content tag|Strong>) marca texto <strong|importante>, e a
  etiqueta <markup|name> (<menu|Insert|Content tag|Name>) marca nomes de
  pessoas, como <name|Joris>. Se mais tarde você decidir que o texto
  importante deve ser exibido em vermelho em vez de negrito, basta mudar o
  estilo, e não os muitos lugares onde <markup|strong> foi usada.

  O exemplo desta seção está em <verbatim|examples/hairy.tm>.

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
