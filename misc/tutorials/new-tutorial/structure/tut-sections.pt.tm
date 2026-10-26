<TeXmacs|2.1.4>

<style|<tuple|tmdoc|portuguese>>

<\body>
  <tmdoc-title|Seções>

  Documentos mais longos são divididos em seções. Para começar uma nova
  seção, selecione <menu|Insert|Section|Section>, ou digite
  <key|\\ s e c t i o n> seguido de <key|return>. O cursor agora está
  dentro de um título de seção vazio, e a barra de status indica
  <verbatim|section>:

  <\big-envbox>
    <section*|1<space|1em><small-focus|<text-cursor>>>
  </big-envbox>

  Digite o título da seção, digamos \PMy new section\Q, e pressione
  <key|return>. O cursor sai do título e vai para um novo parágrafo, onde
  você digita o corpo da seção:

  <\big-envbox>
    <section*|1<space|1em>My new section>

    The body of my section.<text-cursor>
  </big-envbox>

  As seções são numeradas automaticamente, tanto no estilo <tmstyle|generic>
  quanto no estilo <tmstyle|article>. Se
  você inserir uma nova seção antes das existentes, as seções seguintes são
  renumeradas, assim como as referências a elas. A maneira como as seções
  são numeradas e exibidas depende apenas do estilo: quando você passa de
  <tmstyle|article> para <tmstyle|book>, as seções mantêm os seus títulos,
  mas assumem a aparência do estilo de livro.

  Do mesmo modo, <menu|Insert|Section> contém subseções e parágrafos, e no
  estilo <tmstyle|book> <menu|Insert|Chapter|Chapter> começa um novo capítulo. Um
  sumário é gerado com <menu|Insert|Automatic|Table of
  contents>, seguido de <menu|Document|Update|All>.

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
