<TeXmacs|2.1.4>

<style|<tuple|tmdoc|portuguese>>

<\body>
  <tmdoc-title|Teoremas, demonstrações e listas>

  Os textos matemáticos são feitos de definições, teoremas, demonstrações,
  observações e assim por diante. No <TeXmacs>, cada um deles é um
  <em|ambiente>, que se encontra em <menu|Insert|Enunciation>: o <TeXmacs>
  os numera e os exibe de acordo com o estilo do documento. Vamos digitar o
  texto a seguir, que também está disponível em
  <verbatim|examples/ramsey.tm>:

  <\big-envbox>
    <\theorem>
      Among six people, there are always three people who mutually know
      each other or three people who mutually do not know each other.
    </theorem>

    <\proof>
      Let <math|P> be an arbitrary person among the six. Then either one of
      the following cases holds:

      <\description>
        <item*|Case 1><math|P> knows at least three other persons, which
        may be called <math|Q>, <math|R> and <math|S>.

        <item*|Case 2><text-cursor>
      </description>
    </proof>
  </big-envbox>

  <paragraph*|Criando os ambientes>

  Selecione <menu|Insert|Enunciation|Theorem> e digite o enunciado do
  teorema; a palavra \PTeorema\Q e o seu número são acrescentados pelo
  <TeXmacs>. Depois saia do teorema, movendo o cursor para além do seu final
  com <key*|right>, ou com <shortcut|(structured-exit-right)>, e selecione
  <menu|Insert|Enunciation|Proof>. Dentro da demonstração, uma lista
  descritiva é iniciada com <menu|Insert|Description|Default>: cada vez que
  você pressiona <key|return>, um novo item é criado, cujo rótulo você
  digita antes do seu texto.

  <paragraph*|O contexto do cursor>

  Na posição do cursor no exemplo acima, a parte direita da barra de status
  mostra <verbatim|proof description item*>: o cursor está em um item de
  uma descrição, dentro de uma demonstração. Esse <em|contexto> também
  determina o conteúdo do menu <menu|Focus> e da barra de ferramentas de
  foco, que oferecem as ações que fazem sentido para o ambiente mais
  interno.

  <paragraph*|Mudando um ambiente>

  Como o <TeXmacs> sabe que o enunciado é um teorema, é fácil mudar de
  ideia. Coloque o cursor dentro do teorema e pressione
  <shortcut|(variant-circulate (focus-tree) #t)>, ou use a barra de
  ferramentas de foco: o teorema se torna uma proposição, um lema, um
  corolário... enquanto o seu texto permanece o mesmo. Da mesma maneira,
  <shortcut|(numbered-toggle
  (focus-tree))> remove ou restaura o número do teorema.

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
