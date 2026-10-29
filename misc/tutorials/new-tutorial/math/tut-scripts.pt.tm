<TeXmacs|2.1.4>

<style|<tuple|tmdoc|portuguese>>

<\body>
  <tmdoc-title|Índices, expoentes e frações>

  Muitas construções matemáticas, como índices, expoentes, frações e raízes,
  têm uma ou mais partes que você precisa preencher. O <TeXmacs> insere a
  construção com as partes vazias e coloca o cursor na primeira; você então
  digita o seu conteúdo e sai dela quando terminar.

  <paragraph*|Índices e expoentes>

  Para digitar <math|x<rsup|2>>, digite <key|x>, depois <key|^> para um
  expoente, e depois <key|2>. O cursor agora está dentro do expoente, como
  indica a caixa ciano:

  <\big-envbox>
    <math|x<rsup|<small-focus|2<math-cursor>>>>
  </big-envbox>

  Tudo o que você digitar agora vai para o expoente. Pressione <key*|right>
  para mover o cursor para fora do expoente e continue a fórmula. Os índices
  são obtidos da mesma maneira com <key|_>, como em
  <math|x<rsub|n>>.

  <paragraph*|Frações>

  Para continuar a nossa fórmula até <math|x<rsup|2>+<frac|a|b>>, digite
  <key|+> e depois <key|math f> (ou selecione <menu|Insert|Fraction>). Uma
  fração vazia aparece, com o cursor no numerador:

  <\big-envbox>
    <math|x<rsup|2>+<frac|<small-focus|a<math-cursor>>|>>
  </big-envbox>

  Digite <key|a>, depois pressione <key*|down> para ir ao denominador, e
  digite <key|b>:

  <\big-envbox>
    <math|x<rsup|2>+<frac|a|<small-focus|b<math-cursor>>>>
  </big-envbox>

  Por fim, pressione <key*|right> para sair da fração. Da mesma maneira,
  <key|math s> insere uma raiz quadrada, como em <math|<sqrt|x+1>>, e
  <key|math s var> uma raiz <math|n>-ésima, como em <math|<sqrt|x+1|3>>.

  <paragraph*|Removendo uma construção>

  Se você inseriu uma construção por engano, <key*|backspace> no início de
  uma parte vazia a remove. De modo mais geral, o <TeXmacs> acompanha a
  estrutura da sua fórmula: a caixa ciano sempre mostra em qual parte de
  qual construção o cursor está, e a barra de status lista todas as
  construções em torno do cursor, como <verbatim|math> e
  <verbatim|denominator>.

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
